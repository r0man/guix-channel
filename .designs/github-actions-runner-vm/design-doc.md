# Design: VM-backed GitHub Actions runner ("the third way")

## Executive Summary

A new system service type, `github-actions-runner-vm-service-type`, runs each
GitHub Actions runner inside a dedicated QEMU virtual machine whose *entire
operating system* is disposable — the faithful translation of GitHub's hosted
`ubuntu-latest` model. The inner OS (built from a normal Guix `operating-system`
definition) runs the runner agent, a rootful `docker-service-type`, and
exits the whole VM (`poweroff`) when the runner's job is done. The host's
shepherd manages the QEMU process with resource limits; the host never grants
the workflow anything except the QEMU process itself.

Workflows get the full GitHub experience — `services:`, `container:` jobs,
`docker build/run/push` against a real dockerd — with the trust boundary being
the VM (separate kernel, separate filesystem, user-mode networking), not
host-side namespace jails. The existing least-authority process jail remains
available as the cheaper alternative for guix-only workflows; the two designs
compose (the in-VM runner is hardened where practical, but the VM — not the
in-VM user account — is the boundary).

Phased delivery: Phase 1 is the complete MVP — ephemeral VMs with docker
inside (on a per-boot qcow2 scratch disk), the store shared read-only via 9p
(validated machinery — our system-test marionette VMs already boot this way on
both x86_64 and aarch64), and the full PAT-based token lifecycle. Phase 2 is
polish (secrets handling, optional long-lived mode). Phase 3 optimizes guix
inside the VM with a LAN substitute server.

Token minting implementation (corrected): pure Guile, inside the service
module. `(r0man guix services github-actions-vm)' exports
`mint-registration-token', built on Guile core's `(web client)'
`http-request' (POST + Bearer header) and guile-json's `(json)'
`json-string->scm' (added via `with-extensions (list guile-json-4)').
*Note:* `(guix http-client)' was the wrong basis — it only ships GET-style
`http-fetch' and has no `http-request'; Guile's `(web client)' is the right
module and needs no extra dependency. The service type wraps the procedure as
a store program via `program-file' + `with-imported-modules'.  This compiles
into the channel's test suite (SRFI-64 unit test against a local mock HTTP
server, following the runner-script test conventions), fails loudly on
non-200 or missing token field, and needs no JSON-in-sed parsing or curl
dependency.  The minted-token interface (pat-file + url → token file, 0600)
is stable, so a later libsecret PAT backend is a drop-in swap.

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
└── shepherd: github-actions-runner-vm-<N> services (pool, default 2)
    └── qemu-system-<arch> (user networking, -m/-smp limits, no host devices)
        └── inner Guix OS (small per-boot volatile root image + qcow2 scratch)
            ├── 9p: host /gnu/store  → read-only        (guix packages)
            ├── 9p: seed dir         → url/token file    (per-boot credentials)
            ├── 9p: scratch qcow2    → /var/lib/docker, runner work dir (writable)
            ├── guix client → HOST guix-daemon over TCP (slirp 10.0.2.2);
            │   builds run on the host and land in the shared store.
            │   (VM-local daemon + `guix publish` substitutes is the Phase 3
            │    fully-isolated alternative.)
            └── docker + containerd  (root inside VM — fine, VM is the boundary)
                github-actions-runner service (--ephemeral, respawn off,
                exits → poweroff)
```

The VM is the trust boundary: workflow code runs with docker-group privileges
*inside the VM* (needed for the docker socket), but the kernel, filesystem,
and network are the VM's. Escape requires a QEMU/virtio KVM escape — a
categorically stronger position than any socket, namespace, or capability
arrangement on the host.

### Key Components

1. **Inner OS definition** — a Scheme function,
   `github-actions-runner-vm-operating-system`, composing:
   - `%base-services` (or even fewer), `static-networking-service-type`
     (user-mode NIC, `10.0.2.15`, DNS via slirp's 10.0.2.3)
   - `docker-service-type` + `containerd-service-type` (rootful; safe here —
     the VM is the boundary)
   - a **VM variant of the existing runner service** (see the inner-service
     extension below) pointed at the VM-local work dir on the scratch disk,
     `requirement '(dockerd networking)`, `supplementary-groups '("docker")`,
     `extra-registration-args '("--ephemeral")`, and **respawn off**: when the
     runner process exits (job done in `--ephemeral` mode), the start script
     invokes `poweroff -f` — QEMU exits, VM is gone
   - `guix-configuration` substitute/client settings (Phase 3)
2. **Inner runner service variant** (required modification to
   `github-actions-runner-configuration` — the existing service type is
   *reused as a base*, not reused verbatim):
   - `supplementary-groups` field (backward-compatible, default `'()`) so the
     runner user can reach `/var/run/docker.sock` (group `docker`, created by
     Guix's docker service)
   - `ephemeral?` field (default `#f`): adds `--ephemeral` to registration
     args, disables shepherd respawn, and changes the start script to run the
     runner (not `exec`) followed by `poweroff -f` on exit
   - the VM OS passes a configuration with both set; host-side (non-VM) users
     of the service type are unaffected
3. **Host service type** — `github-actions-runner-vm-service-type`:
   - builds `system-qemu-image/shared-store-script` (kernel + initrd + `-append`
     boot, 9p shared store, static networking — the exact mechanism our
     `tests/.../github-actions-system.scm` marionette already proves works on
     x86_64 *and* Asahi aarch64). *Note:* this machinery still builds a small
     (~70 MiB) volatile root disk image per boot; it is not literally
     diskless. The root image is far too small for docker, hence the scratch
     disk below.
   - a **scratch disk**: a sparse qcow2 image (default 32 GiB), created
     per-boot (discarded on poweroff), virtio-blk inside the VM, holding
     `/var/lib/docker` and the runner work dir. This is Phase 1, not an
     option — a `postgis` image cannot fit in the 70 MiB root image, and
     tmpfs-backed docker would eat the memory budget.
   - shepherd service wrapping `qemu-system` with `#:resource-limits`
     (RLIMIT_AS as a *coarse* guard only — with KVM guest RAM is mmap'd so it
     approximates a memory cap, but it bounds virtual address space, not RSS,
     and a too-tight limit breaks QEMU startup; the real caps are the `-m`
     and `-smp` flags) and `#:file-creation-mask`
   - a seed directory 9p-mounted read-only into the VM containing
     `url`/`token` files, so credentials are per-boot, not baked into the
     image; plus a writable marker mapping (see Credential lifecycle)
4. **Credential lifecycle** (decided: ephemeral per job, day one):
   registration tokens are valid for 1 h and are *reusable within that hour*
   (they are NOT single-use — see Security). Each VM boot registers a fresh
   ephemeral runner, so every boot needs a token with remaining validity: a
   host-side program using a GitHub PAT
   (`POST /orgs/{org}/actions/runners/registration-token`) writes it into the
   seed dir before each QEMU start. The host service reuses a still-valid
   token across quick respawns instead of minting a new one per boot (this
   both dampens API rate-limit pressure and is safe since tokens are
   reusable). PAT stored as a plain file (0600, root-owned) or via
   `libsecret`; the VM never sees the PAT, only the minted token. The VM
   start script writes a "registered" marker onto the writable scratch after
   successful registration; the host service deletes the seed token file once
   the marker appears (or after a timeout), shrinking the exposure window.
5. **Guix strategy inside the VM** (decided: shared host daemon):
   store shared 9p **read-only**; the VM's `guix` client reaches the host
   daemon over TCP. Slirp's gateway address 10.0.2.2 already reaches
   host-loopback services, so a `socat` TCP→unix-socket forwarder bound to
   127.0.0.1 is the transport (a socat-less variant relying directly on the
   10.0.2.2 mapping is possible; the socat form is chosen for explicit
   port/ownership control). The VM gets `GUIX_DAEMON_SOCKET=guix://10.0.2.2:<port>`
   via the runner's `environment-variables`. **The TCP daemon is
   unauthenticated** — anything reaching the port can submit builds; slirp
   confines it to the VM, and the forwarder binds 127.0.0.1 only, but local
   host users can also reach a 127.0.0.1 listener (same trust surface the
   jail runner already accepts; documented, not hidden). Builds execute on
   the host and land in the shared store — zero rebuilds. Docker inside the
   VM handles everything docker-shaped. The VM-local daemon plus a
   `guix publish` cache server remains the fully-isolated alternative,
   selectable per instance as a future option (Phase 3).
6. **Configuration record** (data model):
   ```scheme
   (github-actions-runner-vm-configuration
     (os ...)                     ; operating-system or override of the default
     (qemu qemu-minimal)          ; slirp + KVM on both arches; qemu-full not
                                  ; needed (it only adds GUI tools)
     (memory-size 4096)           ; MiB, per VM
     (cpus 2)                     ; per VM
     (scratch-disk-size (* 32 (expt 2 30)))  ; bytes; per-boot qcow2
     (arch ...)                   ; derived from %current-system by default
     (url ...) (pat-file ...)     ; credentials: minted per boot/reuse within 1h
     (labels '("docker" "vm"))
     (parallel-instances 2)       ; pool of N independent ephemeral VMs
     (ephemeral? #t))             ; optional long-lived mode is Phase 2
   ```

### Interface

```scheme
(operating-system
  (services
    (cons* (service github-actions-runner-vm-service-type
                    (github-actions-runner-vm-configuration
                     (url "https://github.com/r0man/burningswell")
                     (pat-file "/etc/github-runner/pat")
                     (labels '("docker" "postgis"))
                     (memory-size 4096)))
           ...)))
```

Host-side control stays shepherd-shaped: one shepherd provision per VM
instance (`github-actions-runner-vm-1` … `-N`, plus a
`github-actions-runner-vm` alias that acts on the pool). `herd stop` on an
instance kills QEMU and *removes the registered runner via the GitHub API
using the PAT*, so a stopped or never-jobed VM does not leave an offline
orphan runner behind (GitHub only auto-removes ephemeral runners *after* a
job). The VM appears in GitHub as an ordinary runner with your chosen labels,
so `runs-on: [self-hosted, docker]` routes to it and the burningswell workflow
runs *unmodified* — `services:` included, since the agent talks to a real
dockerd (inside the VM).

### Lifecycle (decided: shepherd respawn, always-on pool)

GitHub does not push job notifications to self-hosted runners, so the host
service does **not** try to detect queued jobs. Instead:

- The pool VMs are effectively **always on**: after a job finishes, the VM
  powers off, shepherd respawns QEMU immediately, the VM boots (~30–60 s with
  KVM), registers a fresh ephemeral runner, and idles until GitHub dispatches
  the next job.
- Consequence accepted: per-job boot latency (~30–60 s) is paid on every job,
  and idle VMs hold `parallel-instances × memory-size` of RAM continuously.
- Consequence accepted: "ephemeral" means a fresh *OS* per job (the trust
  property), not a VM that is off between jobs.
- Crash handling: if QEMU crashes or the PAT goes bad, minting reuses the
  cached token while it remains valid and shepherd's respawn throttling
  limits the loop; a mint failure logs loudly and backs off rather than
  respawning silently.

## Trade-offs and Decisions

### Decisions Made
- **Guix in the VM**: shared host daemon over TCP (decision 1). No rebuilds;
  the host store is the cache. The workflow's build submissions reach the
  host daemon — accepted, same surface as the jail runner. The daemon-over-TCP
  listener is unauthenticated; mitigations: slirp confinement + 127.0.0.1
  bind only + documented residual risk (local host users).
- **Ephemeral per job, day one** (decision 2): PAT token minting and
  `--ephemeral` registration are Phase 1, not deferred.
- **Fixed pool of N VMs** (decision 3): `parallel-instances` config field,
  default 2, each an independent ephemeral VM with its own shepherd provision.
- **User-mode (slirp) networking** (decision 4): outbound-only, no host
  network configuration, no inbound.
- **Reuse, don't fork, the existing runner code — but extend it**: the VM's
  inner service is built on `github-actions-runner-service-type` with the new
  `supplementary-groups` and `ephemeral?` fields (backward-compatible
  additions to the existing module). The VM service is a *host manager* for
  QEMU, not a second runner implementation; the inner service is *not* used
  verbatim (docker-socket access, no-respawn, and poweroff-on-exit require
  the variant).
- **Boot mechanism**: `-kernel` boot with 9p read-only store plus a small
  per-boot volatile root image (70 MiB) — the machinery exercised by the
  channel's system test on both architectures — *plus* a per-boot qcow2
  scratch disk for docker and the work dir (the root image is far too small
  for docker). No long-lived image to maintain; state restored from seed
  files at boot.
- **Docker inside the VM, rootful** — the VM is the boundary; we replicate
  GitHub's actual model instead of half-measures (no host socket ever).
- **Credentials via 9p seed dir** (RO into VM), not image-baked. Registration
  tokens are reusable within their 1 h validity; the host service reuses a
  cached token across quick respawns and deletes the seed token file once the
  VM's "registered" marker appears.
- **Token minting in pure Guile** (decided): `mint-registration-token` lives
  in the service module on Guile core `(web client)` `http-request` (POST +
  Bearer) + guile-json `(json)` — compiled, unit-tested against a mock API
  server in `make check`, no curl/sed JSON parsing. `(guix http-client)` is
  GET-only and cannot POST. Wrapped for the store via `program-file' +
  `with-extensions (list guile-json-4)'.

### Open Questions
All four resolved 2026:
1. Daemon sharing → shared host daemon over TCP (unauthenticated; slirp +
   127.0.0.1 bind; documented).
2. Ephemeral semantics → ephemeral per job, day one (PAT minting in Phase 1).
3. Concurrency → fixed pool of N VMs (`parallel-instances`, default 2).
4. Networking → QEMU user-mode (slirp), outbound-only.

### Trade-offs
- **vs. in-process least-authority runner**: +VM-grade isolation, unmodified
  docker workflows, per-job disposability; −heavier (memory/boot, always-on
  pool), slower guix cold paths, more moving parts (QEMU, scratch disk, seed
  dir, token lifecycle), per-job boot latency (~30–60 s).
- **vs. GitHub-hosted**: identical model, but *you* pay compute and manage
  tokens; in exchange, full channel control and your pinned guix everywhere.

## Risks and Mitigations
- **QEMU/9p vulnerabilities** → user-mode networking only (no LAN bridge),
  store read-only, minimal `-device` set, shepherd resource limits (as coarse
  guards; `-m`/`-smp` are the real caps); keep QEMU current via guix.
- **Token leakage**: the seed token is mounted read-only into the VM and
  registration tokens are *reusable for 1 h* — workflow code with docker
  privileges inside the VM can read it and register additional rogue runners
  for up to an hour. Mitigations: the host service deletes the seed token
  file as soon as the VM's "registered" marker appears on the scratch disk
  (shrinking the window to roughly boot+register time); tokens expire after
  1 h; monitor unexpected runner registrations; PAT never crosses into the
  VM. Residual risk accepted and documented — if a stronger answer is needed,
  the fallback is bootstrapping credentials over the host-side tunnel instead
  of the seed mount.
- **Runaway VMs** (workflow forks bomb): `-m`/`-smp` caps (the real limits);
  RLIMIT_AS as a coarse extra guard; scratch-disk size cap; hard host-side
  wall-clock supervision option in the service (Phase 2).
- **Store-sharing regression**: 9p mount is RO (`mapping->file-system` sets
  `(flags '(read-only))` for the store mapping — verified in
  `gnu/system/vm.scm`); a corrupted/compromised VM cannot write the store —
  verified by design, assert mount flags at boot.
- **Scratch disk growth**: qcow2 is sparse and recreated per boot, so growth
  is bounded by `scratch-disk-size` and reset on every job.
- **Diskless-ish root means no persistence**: intended; the only writable
  state is the per-boot scratch disk.
- **Unauthenticated guix daemon over TCP**: confined to slirp + 127.0.0.1;
  same build-submission trust surface the jail runner already accepts;
  documented for the host admin.

## Known Environment Issues (recorded during implementation, 2026-09-20)
- The `run-system-tests.scm` loader of this dev-profile Guix fails with
  `Wrong type to apply: #<syntax-transformer current-target-system>` for
  *all* system tests, including the pre-existing
  `github-actions-system.scm` one (upstream guix/guile-next interaction:
  `define-inlinable` bindings and deprecated package aliases such as
  `oniguruma` resolve to syntax transformers when package modules are
  loaded from source in the loader's fresh module context).  Not caused
  by this feature.  Workaround: the VM smoke test
  (`scripts/github-actions-vm-smoke-test`) boots the *same* inner
  operating system directly through the user's compiled `guix` CLI,
  which works.  The `make check-system` run should be retried once the
  profile is rebuilt.

## Implementation Plan

### Phase 1: MVP (proves the model)
1. **Token machinery first** (proves the riskiest new behavior before any VM
   plumbing): implement `mint-registration-token` in
   `(r0man guix services github-actions-vm)` on Guile core `(web client)`
   `http-request` + guile-json `(json)`, with a SRFI-64 unit test that runs a
   local mock API server, asserting the request path, POST method, Bearer
   auth header, and token extraction — wired into `make check` like the
   existing runner-script tests.
2. **Extend the runner configuration module**
   (`modules/r0man/guix/services/github-actions.scm`): add backward-compatible
   `supplementary-groups` and `ephemeral?` fields (the latter: `--ephemeral`
   registration arg, respawn off, run-then-`poweroff -f` script). Existing
   non-VM users see no behavior change.
3. `modules/r0man/guix/services/github-actions-vm.scm`: inner-OS constructor +
   host service type. Ephemeral lifecycle from the start: token minting via
   PAT (step 1) with cached-token reuse within its 1 h validity, `--ephemeral`
   runner registration, VM power-off on job end, immediate shepherd respawn,
   per-boot qcow2 scratch disk, 9p RO store, shared host daemon over
   127.0.0.1 TCP via slirp 10.0.2.2, docker inside. Pool of
   `parallel-instances` (default 2) VMs, each with its own seed dir and
   shepherd provision; `herd stop` removes the registered runner via the API.
   Mint failures: log loudly + back off.
4. System test: extend the existing marionette harness — booting the *VM
   service* inside the test VM (nested, KVM-less) is too deep; instead test
   that the service derives a runnable `qemu` script, that the inner OS
   builds, and that the minting unit test passes; plus a smoke marionette
   boot of the inner OS asserting dockerd + runner services come up (the
   runner reports "not registered" like the existing test — no network to
   GitHub needed).
5. Manual E2E on burningswell: run its CI workflow verbatim; confirm
   `services: postgres` green.

**Prerequisite (user)**: a GitHub fine-grained PAT with `administration:write`
scope on the target repo(s)/org, stored at the configured `pat-file` (0600,
root-owned).

### Phase 2: Polish
Secrets hardening for the PAT (libsecret or strict file handling), host-side
wall-clock supervision, optional long-lived (non-ephemeral) VM mode as a
configuration option (already partly shaped by the `ephemeral?` field),
optional tightening of the token exposure window (credential bootstrap over
the host tunnel instead of the seed mount).

### Phase 3: Guix inside the VM
`guix publish` host service + VM substitute config; optional VM-local daemon
(the fully-isolated alternative); aarch64/x86_64 matrix; optional home-service
variant (QEMU is unprivileged and slirp needs no host privileges, but the
shared guix daemon is a host root service — the home variant would need its
own daemon or substitute-only access).

## Appendix: Dimension Notes (inline, medium scope)
- **api**: one new service type + record; the existing runner configuration
  gains two backward-compatible fields; VM-internal reuse of the runner
  service keeps new script surface minimal.
- **data**: seed dir (url/token, deleted after registration), per-boot qcow2
  scratch disk, per-boot volatile root image; no host state beyond shepherd
  logs and the cached minted token.
- **security**: VM = kernel boundary; host exposure = one qemu process (slirp)
  + one unauthenticated guix-daemon TCP listener on 127.0.0.1; RO store;
  reusable registration token exposed to the VM for the boot+register window
  (documented, minimized); PAT never enters the VM.
- **scale**: boot ~30–60 s (KVM) paid per job; pool is always-on
  (`parallel-instances × memory-size` RAM held continuously); cold guix
  in-VM is fast via the shared daemon; docker I/O is bounded by the scratch
  disk and goes through slirp for pulls (slow but outbound-only); per-VM
  memory is the scaling wall.
- **integration**: shares the runner package and service code; the existing
  in-process service gains only additive fields; works on Asahi aarch64
  (validated boot path).
- **ux**: workflows unchanged (this is the design goal); host admins see a
  small pool of shepherd services; debugging via serial console (`-serial`
  logged by shepherd).
