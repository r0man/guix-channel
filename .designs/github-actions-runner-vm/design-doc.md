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

Phased delivery: Phase 1 is the complete MVP — ephemeral VMs with docker
inside, the store shared read-only via 9p (validated machinery — our
system-test marionette VMs already boot this way on both x86_64 and
aarch64), and the full PAT-based token lifecycle. Phase 2 is polish
(secrets handling, optional long-lived mode). Phase 3 optimizes guix
inside the VM with a LAN substitute server.

Token minting implementation (decided): pure Guile, inside the service
module. `(r0man guix services github-actions-vm)' exports
`mint-registration-token', a procedure built on `(guix http-client)' and
`json-string->scm' — both already shipped by Guix (verified: `http-request',
guile-json parsing, and `Authorization' header plumbing all exist). The
service type wraps it as a store program via `program-file' +
`with-extensions (list guile-json-4)' + `with-imported-modules'.  This
compiles into the channel's test suite (SRFI-64 unit test against a local
mock HTTP server, following the runner-script test conventions), fails
loudly on non-200 or missing token field, and needs no JSON-in-sed parsing
or curl dependency.  The minted-token interface (pat-file + url → token
file, 0600) is stable, so a later libsecret PAT backend is a drop-in
swap.

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
4. **Credential lifecycle** (decided: ephemeral per job, day one):
   registration tokens expire after 1 h, and each VM boot registers a fresh
   ephemeral runner, so every boot needs a freshly minted token: a host-side
   program using a GitHub PAT
   (`POST /orgs/{org}/actions/runners/registration-token`), writing it into
   the seed dir before each QEMU start. PAT stored as a plain file (0700,
   root-owned) or via `libsecret`; the VM never sees the PAT, only the
   minted token.
5. **Guix strategy inside the VM** (decided: shared host daemon):
   store shared 9p **read-only**; the VM's `guix` client reaches the host
   daemon over TCP (a loopback-only `socat`/forwarder started by the host
   service, exposed into the VM via user-mode networking's host-gateway).
   Builds execute on the host and land in the shared store — zero rebuilds,
   same daemon-sharing trust surface the jail runner already has. Docker
   inside the VM handles everything docker-shaped. The VM-local daemon plus
   a `guix publish` cache server remains the fully-isolated alternative,
   selectable per instance as a future option.

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
- **Guix in the VM**: shared host daemon over TCP (decision 1). No rebuilds;
  the host store is the cache. The workflow's build submissions reach the
  host daemon — accepted, same surface as the jail runner.
- **Ephemeral per job, day one** (decision 2): PAT token minting and
  `--ephemeral` registration are Phase 1, not deferred.
- **Fixed pool of N VMs** (decision 3): `parallel-instances` config field,
  default 2, each an independent ephemeral VM.
- **User-mode (slirp) networking** (decision 4): outbound-only, no host
  network configuration, no inbound.
- **Reuse, don't fork, the existing service**: the inner VM runs our
  `github-actions-runner-service-type` verbatim; the VM service is a *host
  manager* for QEMU, not a second runner implementation.
- **Boot mechanism**: `-kernel`/diskless with 9p store — already exercised by
  the channel's system test on both architectures; no image building or
  maintenance, state restored from seed files at boot.
- **Docker inside the VM, rootful** — the VM is the boundary; we replicate
  GitHub's actual model instead of half-measures (no host socket ever).
- **Credentials via 9p seed dir** (RO into VM), not image-baked.
- **Token minting in pure Guile** (decided): `mint-registration-token` lives
  in the service module on `(guix http-client)` + guile-json — compiled,
  unit-tested against a mock API server in `make check`, no curl/sed JSON
  parsing. Wrapped for the store via `program-file' + `with-extensions`.

### Open Questions
All four resolved 2026:
1. Daemon sharing → shared host daemon over TCP.
2. Ephemeral semantics → ephemeral per job, day one (PAT minting in Phase 1).
3. Concurrency → fixed pool of N VMs (`parallel-instances`, default 2).
4. Networking → QEMU user-mode (slirp), outbound-only.

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
1. **Token machinery first** (proves the riskiest new behavior before any VM
   plumbing): implement `mint-registration-token` in
   `(r0man guix services github-actions-vm)` on `(guix http-client)` +
   guile-json, with a SRFI-64 unit test that runs a local mock API server,
   asserting the request path, Bearer auth header, and token extraction —
   wired into `make check` like the existing runner-script tests.
2. `modules/r0man/guix/services/github-actions-vm.scm`: inner-OS constructor +
   host service type. Ephemeral lifecycle from the start: token minting via
   PAT (step 1), `--ephemeral` runner registration, VM power-off on job end,
   respawn on the next queued job. 9p store, shared host daemon, slirp
   networking, docker inside. Pool of `parallel-instances` (default 2) VMs,
   each with its own seed dir and per-boot minted token.
3. System test: extend the existing marionette harness — boot the *VM service*
   inside the test VM (nested, KVM-less) is too deep; instead test that the
   service derives a runnable `qemu` script and that the inner OS builds
   (`guix system vm`-style build check), plus a smoke marionette boot of the
   inner OS asserting dockerd + runner services come up.
4. Manual E2E on burningswell: run its CI workflow verbatim; confirm
   `services: postgres` green.

**Prerequisite (user)**: a GitHub fine-grained PAT with `administration:write`
scope on the target repo(s)/org, stored at the configured `pat-file` (0600,
root-owned).

### Phase 2: Polish
Secrets hardening for the PAT (libsecret or strict file handling),
per-job scratch cleanup, optional long-lived (non-ephemeral) VM mode as a
configuration option, host-side wall-clock supervision.

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
