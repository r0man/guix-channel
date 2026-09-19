# Design: VM-backed GitHub Actions runner ("the third way")

## Executive Summary

A new system service type, `github-actions-runner-vm-service-type`, runs each
GitHub Actions runner inside a dedicated QEMU virtual machine whose *entire
operating system* is disposable — the faithful translation of GitHub's hosted
`ubuntu-latest` model. The inner OS (built from a normal Guix `operating-system`
definition) runs the runner agent, a rootful `docker-service-type`, and a
supervisor that powers the VM off when the runner exits. The host's shepherd
manages the QEMU process with resource limits; the host never grants the
workflow anything except the QEMU process itself.

Workflows get the full GitHub experience — `services:`, `container:` jobs,
`docker build/run/push` against a real dockerd — with the trust boundary being
the VM (separate kernel, separate filesystem, user-mode networking), not
host-side namespace jails. The existing least-authority process jail remains
available as the cheaper alternative for guix-only workflows; the two designs
compose (the in-VM runner can still run hardened).

Phased delivery: Phase 1 is a long-lived VM with docker inside and the store
shared read-only via 9p (validated machinery — our system-test marionette VMs
already boot this way on both x86_64 and aarch64). Phase 2 adds the ephemeral
lifecycle (fresh registration per job, PAT-based token minting, boot-per-job).
Phase 3 optimizes guix inside the VM with a LAN substitute server.

## Problem Statement

Workflows that need docker (the `services:`/`container:` idioms, e.g.
burningswell's `postgis/postgis:15-3.4` service) cannot run in the
least-authority runner: no docker socket may cross the jail, and granting
rootful docker on the host would make the jail decorative. We want GitHub
Actions' docker semantics *without* weakening the persistent host.

## Proposed Design

### Overview

```
HOST (Guix System, hardened, untouched)
└── shepherd: github-actions-runner-vm service
    └── qemu-system-<arch> (user networking, -m/-smp limits, no host devices)
        └── inner Guix OS (diskless root or small qcow2)
            ├── 9p: host /gnu/store  → read-only        (guix packages)
            ├── 9p: seed dir         → url/token file    (per-boot credentials)
            ├── guix-daemon (own store cache, optional substitutes)
            ├── docker + containerd  (root inside VM — fine, VM is the boundary)
            └── github-actions-runner service (--ephemeral in Phase 2)
```

The VM is the trust boundary: workflow code runs as root *inside the VM*
(needed for docker), but the kernel, filesystem, and network are the VM's.
Escape requires a QEMU/virtio KVM escape — a categorically stronger position
than any socket, namespace, or capability arrangement on the host.

### Key Components

1. **Inner OS definition** — a Scheme function,
   `github-actions-runner-vm-operating-system`, composing:
   - `%base-services` (or even fewer), `static-networking-service-type`
     (user-mode NIC, `10.0.2.15`)
   - `docker-service-type` + `containerd` (rootful; safe here)
   - the existing `github-actions-runner-service-type` (unchanged!) pointed at
     the VM-local work dir, `requirement '(dockerd networking user-processes)`
   - an `on-exit`/shepherd supervision hook: when the runner service stops
     (job done in `--ephemeral` mode), `poweroff` — QEMU exits, VM is gone
   - optional `guix-publish` substitute server on the host listed in the VM's
     `guix-configuration` (Phase 3)
2. **Host service type** — `github-actions-runner-vm-service-type`:
   - builds `system-qemu-image/shared-store-script` (kernel + initrd + `-append`
     boot, 9p shared store, static networking — the exact mechanism our
     `tests/.../github-actions-system.scm` marionette already proves works on
     x86_64 *and* Asahi aarch64)
   - shepherd service wrapping `qemu-system` with `#:resource-limits`
     (RLIMIT_AS for the emulator) and `#:file-creation-mask`
   - a seed directory 9p-mounted read-only into the VM containing
     `url`/`token` files, so credentials are per-boot, not baked into the image
3. **Configuration record** (data model):
   ```scheme
   (github-actions-runner-vm-configuration
     (os ...)              ; operating-system or override of the default one
     (qemu qemu-minimal)   ; or qemu-full for GUI-less acceleration choices
     (memory-size 4096)    ; MiB
     (cpus 2)
     (arch (or "x86_64" "aarch64"))  ; derived from %current-system by default
     (url ...) (token-file ...)      ; seed credentials, read via 9p
     (labels '("docker" "vm"))
     (ephemeral? #f)                 ; Phase 2: boot-per-job
     (pat-file #f))                  ; Phase 2: for token minting
   ```
4. **Credential lifecycle** (the real hard problem, deferred to Phase 2):
   registration tokens expire after 1 h, so boot-per-job requires minting a
   fresh token per boot: a host-side program using a GitHub PAT
   (`POST /orgs/{org}/actions/runners/registration-token`), writing it into
   the seed dir before each QEMU start. PAT stored as a plain file or via
   `libsecret`; the VM never sees the PAT, only the minted token.
5. **Guix strategy inside the VM** (decision, see Trade-offs):
   store shared 9p **read-only**; `GUIX_DAEMON_SOCKET` → host daemon over TCP
   (`socat`/`guix publish`-style forwarder on a loopback-only port) — same
   daemon-sharing boundary we already accepted in the jail design; docker
   inside the VM handles everything docker-shaped. VM-local `guix-daemon`
   remains the fully-isolated alternative, selectable per instance.

### Interface

```scheme
(operating-system
  (services
    (cons* (service github-actions-runner-vm-service-type
                    (github-actions-runner-vm-configuration
                     (url "https://github.com/r0man/burningswell")
                     (token-file "/etc/github-runner/registration-token")
                     (labels '("docker" "postgis"))
                     (memory-size 4096)))
           ...)))
```

Host-side control stays shepherd-shaped: `herd start/stop github-actions-runner-vm`;
the VM appears in GitHub as an ordinary runner with your chosen labels, so
`runs-on: [self-hosted, docker]` routes to it and the burningswell workflow
runs *unmodified* — `services:` included, since the agent talks to a real
dockerd (inside the VM).

## Trade-offs and Decisions

### Decisions Made
- **Reuse, don't fork, the existing service**: the inner VM runs our
  `github-actions-runner-service-type` verbatim; the VM service is a *host
  manager* for QEMU, not a second runner implementation.
- **Boot mechanism**: `-kernel`/diskless with 9p store — already exercised by
  the channel's system test on both architectures; no image building or
  maintenance, state restored from seed files at boot.
- **Docker inside the VM, rootful** — the VM is the boundary; we replicate
  GitHub's actual model instead of half-measures (no host socket ever).
- **Credentials via 9p seed dir** (RO into VM), not image-baked.

### Open Questions (need human input)
1. **Daemon sharing**: host-daemon-over-TCP (fast, same daemon abuse surface as
   today) vs VM-own-daemon (max isolation, custom packages rebuilt in-VM). For
   burningswell's nonguix/custom SBCL packages this is a real cost either way;
   `guix publish` on the LAN (Phase 3) mitigates. Which trade do you want?
2. **Ephemeral semantics**: one job per VM boot (cleanest, matches GitHub
   hosted; costs a boot + substitute warm-up per job, needs PAT minting) vs
   long-lived VM with periodically recycled runner. Boot cost on this Asahi
   host is ~1–2 min with KVM.
3. **Concurrency**: N VMs = N × (memory, smp). Default 1 VM; scaling policy
   later.
4. **Networking mode**: slirp user-mode (default, zero host config, no inbound)
   vs tap bridge (only if a workflow must *receive* connections).

### Trade-offs
- **vs. in-process least-authority runner**: +VM-grade isolation, unmodified
  docker workflows, per-job disposability; −heavier (memory/boot), slower guix
  cold paths, more moving parts (QEMU, seed dir, token lifecycle).
- **vs. GitHub-hosted**: identical model, but *you* pay compute and manage
  tokens; in exchange, full channel control and your pinned guix everywhere.

## Risks and Mitigations
- **QEMU/9p vulnerabilities** → user-mode networking only (no LAN bridge),
  store read-only, minimal `-device` set, shepherd resource limits; keep QEMU
  current via guix.
- **Token leakage**: seed dir is 0700 root; minted tokens are single-use and
  short-lived; PAT lives only on host, ideally in a secrets manager.
- **Runaway VMs** (workflow forks bomb): cgroup-free host limits via
  `resource-limits`; VM-internal `--cpus/-m` caps; hard host-side wall-clock
  supervision option in the service.
- **Store-sharing regression**: 9p mount is RO; a corrupted/compromised VM
  cannot write the store — verified by design, assert mount flags at boot.
- **Diskless root means no persistence**: intended; anything needing
  persistence goes through the 9p writable scratch mapping (bounded quota via
  a qcow2 backing instead, selectable).

## Implementation Plan

### Phase 1: MVP (proves the model)
1. `modules/r0man/guix/services/github-actions-vm.scm`: inner-OS constructor +
   host service type (long-lived VM, static token via seed dir, 9p store,
   slirp networking, docker inside).
2. System test: extend the existing marionette harness — boot the *VM service*
   inside the test VM (nested, KVM-less) is too deep; instead test that the
   service derives a runnable `qemu` script and that the inner OS builds
   (`guix system vm`-style build check), plus a smoke marionette boot of the
   inner OS asserting dockerd + runner services come up.
3. Manual E2E on burningswell: run its CI workflow verbatim; confirm
   `services: postgres` green.

### Phase 2: Ephemeral lifecycle
PAT-based registration-token minting, `--ephemeral`, VM power-off on job end,
respawn policy, secrets handling for the PAT, per-job scratch cleanup.

### Phase 3: Guix inside the VM
`guix publish` host service + VM substitute config; optional VM-local daemon;
aarch64/x86_64 matrix; optional home-service variant (QEMU is unprivileged —
the whole stack can run under a home shepherd with slirp).

## Appendix: Dimension Notes (inline, medium scope)
- **api**: single service type + record; VM-internal reuse of existing runner
  service keeps the public API to one new type and no new script surface.
- **data**: seed dir (url/token), optional qcow2 scratch; no host state beyond
  shepherd logs.
- **security**: VM = kernel boundary; host exposure = one qemu process (slirp);
  RO store; no host filesystem mappings beyond seed + store.
- **scale**: boot ~30–60 s (KVM); cold guix in-VM substitutable packages are
  fast, custom packages need Phase 3; per-VM memory is the scaling wall.
- **integration**: shares the runner package and service code; zero changes to
  the existing in-process services; works on Asahi aarch64 (validated boot
  path).
- **ux**: workflows unchanged (this is the design goal); host admins see one
  shepherd service; debugging via serial console (`-serial` logged by
  shepherd).
