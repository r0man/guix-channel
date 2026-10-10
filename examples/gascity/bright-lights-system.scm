;;; An interactive, ephemeral system Gas City demo: a fresh "bright-lights" city.
;;;
;;; It runs one machine-wide Gas City supervisor with a single city named
;;; "bright-lights" that uses the builtin "claude" provider, the same minimal
;;; shape as examples/gascity/system.scm.  The city also declares the
;;; tutorial's project rig /var/lib/gascity/my-project (the service user's
;;; `~').  Because the rig is declared in the city config, the service
;;; initializes its beads store itself, as the gascity user, when the city
;;; starts -- `gc sling my-project/claude ...' works without running
;;; `gc rig add'.  (Do not run `gc rig add' as root: it would create
;;; root-owned beads files that the gascity supervisor cannot read.)  On top
;;; of that it puts the Claude
;;; Code CLI on both the supervisor's agent PATH (so `gc sling' can spawn a
;;; claude agent) and the system PATH (so `claude' can be run by hand), and it
;;; gives the service user a Claude Code configuration of its own at boot.
;;;
;;; The agents never see the operator's own Claude Code login.  The wrapper
;;; exposes a dedicated Console API key read-only
;;; (`scripts/gascity-system-container' passes `--expose'), and a one-shot
;;; service installs it, with a minimal generated `~/.claude.json', into the
;;; service user's home before the supervisor starts.  Without a key the city
;;; boots unauthenticated.
;;;
;;; Everything else uses the service defaults: the supervisor runs as the
;;; "gascity" account, its state directory is /var/lib/gascity (the account
;;; home), its log directory /var/log/gascity, and its GC_HOME
;;; /var/lib/gascity/.gc.
;;;
;;; The operating system inherits `simple-operating-system' (from (gnu tests),
;;; the small OS used throughout Guix's own tests) so the file is directly
;;; runnable.  Build a container or a VM:
;;;
;;;   guix system build     -L modules examples/gascity/bright-lights-system.scm
;;;   guix system container -L modules examples/gascity/bright-lights-system.scm
;;;   guix system vm        -L modules examples/gascity/bright-lights-system.scm
;;;
;;; scripts/gascity-system-container wraps the VM invocation, exposes the host
;;; Claude Code state read-only, and boots the result.  On aarch64 the
;;; generated launcher must also be given QEMU's machine and CPU (the script
;;; does this; see below).
;;;
;;; The VM console is architecture-dependent.  On x86_64 Guix defaults to
;;; `console=ttyS0' and QEMU's default machine wires it to the serial port; on
;;; aarch64 `guix system vm' still passes `console=ttyS0', but QEMU's `virt'
;;; machine wires its PL011 serial device to `ttyAMA0', so the console stays
;;; empty.  The OS therefore selects `ttyAMA0' on aarch64 (see
;;; `%kernel-arguments') and runs a serial `agetty' on that tty, so the VM
;;; presents a `login:' prompt on the console:
;;;
;;;   $(guix system vm -L modules examples/gascity/bright-lights-system.scm) \
;;;       -M virt -cpu max
;;;
;;; Log in as `root' with no password (a fresh Guix System leaves the root
;;; account passwordless).  The system container smoke test and the opt-in
;;; make target are added separately (see docs/gascity-service.org).

(use-modules (gnu)
             (gnu bootloader)
             (gnu bootloader u-boot)
             (gnu packages version-control)
             (gnu services)
             (gnu services base)
             (gnu services shepherd)
             (gnu system)
             (gnu system file-systems)
             (gnu system linux-initrd)
             (gnu tests)
             (guix gexp)
             (guix utils)
             (r0man guix packages claude)
             (r0man guix services gascity)
             (r0man guix toml))

;;; The single fresh city, mirroring examples/gascity/system.scm.
(define %city
  (gascity-city-configuration
   (name "bright-lights")
   ;; Fetch nothing: the example is self-contained and offline.
   (install-packs? #f)
   (workspace (gascity-workspace-configuration
               (name "bright-lights")
               (provider "claude")))
   (providers
    (list (cons "claude"
                (gascity-provider-configuration
                 (base "builtin:claude")))))
   ;; The tutorial's project rig, declared so it has a name, prefix and path.
   ;; The service runs as the "gascity" account, so its `~' is the state
   ;; directory: /var/lib/gascity/my-project.
   (rigs
    (list
     (gascity-rig-configuration
      (name "my-project")
      (path "/var/lib/gascity/my-project")
      (prefix "mp"))))
   (daemon (gascity-daemon-configuration
            (patrol-interval "30s")
            (max-restarts 5)))
   (pack (gascity-pack-configuration
          (name "bright-lights")
          (schema 2)
          ;; The builtin `core' and `bd' packs provide the bead script
          ;; `gc init' runs at genesis.  The gc binary pre-seeds these pinned
          ;; versions offline, so `install-packs? #f' still fetches nothing.
          (imports
           (list
            (cons "core"
                  (gascity-import-configuration
                   (source "https://github.com/gastownhall/gascity.git//internal/bootstrap/packs/core")
                   (version "sha:f895c0ff47d6ee9334ed282a416387eb5b084d24")))
            (cons "bd"
                  (gascity-import-configuration
                   (source "https://github.com/gastownhall/gascity.git//examples/bd")
                   (version "sha:f895c0ff47d6ee9334ed282a416387eb5b084d24")))))))
   ;; The extra-config escape hatch adds tables that have no typed record yet;
   ;; both are known to the city schema.
   (extra-config
    (list (toml-table 'mail
                      (toml-field 'provider "smtp")
                      (toml-field 'retention_ttl "24h"))
          (toml-table 'api
                      (toml-field 'bind "127.0.0.1"))))))

;;; Normalize the supervisor once so the derived paths (state directory,
;;; gc-home, log file) referenced below match exactly what the service uses.
;;; `claude-code' on the supervisor's `packages' puts the CLI on its agent
;;; PATH so spawned claude sessions resolve the binary by name.
(define %supervisor
  (car
   (gascity-supervisor-configurations
    (list (gascity-supervisor-configuration
           ;; Dolt requires a commit author identity for managed bd storage;
           ;; without one `gc init' fails at genesis.
           (dolt-user-name "Gas City")
           (dolt-user-email "gascity@example.com")
           ;; The container shares the host network namespace, where a host
           ;; supervisor usually owns 8372, so bind the demo elsewhere;
           ;; override with GASCITY_DEMO_PORT at build time.
           (port (or (and=> (getenv "GASCITY_DEMO_PORT") string->number)
                     18372))
           (packages (list claude-code))
           (cities (list %city)))))))

(define %service-user (gascity-supervisor-user %supervisor))
(define %service-home (gascity-supervisor-state-directory %supervisor))

;;; The exposed, read-only Claude Code API key.  `scripts/gascity-system-container'
;;; stages the file named by `GASCITY_ANTHROPIC_API_KEY_FILE' -- a Console API
;;; key created for this city alone, with a spend limit -- as `anthropic-api-key'
;;; in a private directory and mounts it here (`--expose' can only export a
;;; directory).  The operator's own `~/.claude/.credentials.json' and
;;; `~/.claude.json' are never shared: the agents run with
;;; `--dangerously-skip-permissions' on untrusted input and can read anything
;;; their user can, so they get a credential that is theirs alone and cheap to
;;; revoke.
(define %claude-source "/mnt/claude-ro")

;;; Claude Code remembers which directories it may work in
;;; (`projects[PATH].hasTrustDialogAccepted' in `~/.claude.json'); without it,
;;; the first start in a fresh service-user home shows the trust dialog, whose
;;; default answer ("No, exit") makes a non-interactive `gc sling' session
;;; quit on startup.  The generated `~/.claude.json' trusts every declared rig
;;; path and nothing else.
(define %rig-paths
  (map gascity-rig-configuration-path
       (gascity-city-configuration-rigs %city)))

;;; Install the Claude Code configuration into the service user's home.  The
;;; one-shot runs as root (Shepherd services do by default), but the home
;;; belongs to the service user, whose agents could plant symbolic links in
;;; it, so root only reads the exposed key and then forks and drops to the
;;; service user before it writes anything.  It writes:
;;;
;;;   - `~/.claude.json': onboarding done, plus the rig trust entries;
;;;   - when a key was exposed, `~/.claude/anthropic-api-key' (0600) and a
;;;     `~/.claude/settings.json' whose `apiKeyHelper' prints it.
;;;
;;; Without a key the city still boots; run `claude login' as the service
;;; user with an account of its own.
(define %claude-credentials-program
  (program-file
   "gascity-claude-credentials"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match)
                      (ice-9 textual-ports))

         (define home #$%service-home)
         (define key-file (string-append home "/.claude/anthropic-api-key"))

         (define (json-string string)
           ;; Rig paths and key paths are plain; quote and escape minimally.
           (string-append "\""
                          (string-concatenate
                           (map (lambda (character)
                                  (case character
                                    ((#\") "\\\"")
                                    ((#\\) "\\\\")
                                    (else (string character))))
                                (string->list string)))
                          "\""))

         (define claude-json
           (string-append
            "{\n"
            "  \"hasCompletedOnboarding\": true,\n"
            "  \"projects\": {\n"
            (string-join
             (map (lambda (path)
                    (string-append "    " (json-string path)
                                   ": { \"hasTrustDialogAccepted\": true }"))
                  '#$%rig-paths)
             ",\n")
            "\n  }\n"
            "}\n"))

         (define settings-json
           (string-append
            "{\n"
            "  \"apiKeyHelper\": "
            (json-string (string-append "cat " key-file)) "\n"
            "}\n"))

         (define (write-file file content)
           ;; Atomically, as the service user, readable only by them.
           (let ((temporary (string-append file ".tmp")))
             (when (file-exists? temporary)
               (delete-file temporary))
             (call-with-output-file temporary
               (lambda (port) (display content port)))
             (chmod temporary #o600)
             (rename-file temporary file)))

         ;; Root reads the exposed key...
         (define key
           (let ((source (string-append #$%claude-source
                                        "/anthropic-api-key")))
             (and (file-exists? source)
                  (let ((key (string-trim-both
                              (call-with-input-file source get-string-all))))
                    (and (not (string-null? key)) key)))))

         ;; ...and writes nothing itself.  The account exists already:
         ;; account activation runs before services.
         (let ((account (getpwnam #$%service-user)))
           (match (primitive-fork)
             (0
              (catch #t
                (lambda ()
                  (setgroups #())
                  (setgid (passwd:gid account))
                  (setuid (passwd:uid account))
                  (umask #o077)
                  (mkdir-p (string-append home "/.claude"))
                  (chmod (string-append home "/.claude") #o700)
                  (write-file (string-append home "/.claude.json") claude-json)
                  (when key
                    (write-file key-file (string-append key "\n"))
                    (write-file (string-append home "/.claude/settings.json")
                                settings-json))
                  (primitive-exit 0))
                (lambda args
                  (format (current-error-port)
                          "gascity-claude-credentials: ~s~%" args)
                  (force-output (current-error-port))
                  (primitive-exit 1))))
             (pid
              (match (waitpid pid)
                ((_ . status) (exit (zero? status)))))))))))

;;; The install runs after the `--expose' mounts.  In a VM those are 9p file
;;; systems mounted by the file-system services, so a one-shot service (not
;;; activation, which runs before any file system is mounted) is required.
;;; Extending `user-processes' below makes the supervisor, which requires
;;; `user-processes', start only after the credentials are in place.
(define %claude-credentials-service
  (shepherd-service
   (documentation
    "Install the Claude Code configuration into the service user's home.")
   (provision '(gascity-claude-credentials))
   (requirement '(file-systems))
   (one-shot? #t)
   (start #~(lambda _
              (zero? (system* #$%claude-credentials-program))))
   (stop #~(const #f))
   (respawn? #f)))

;;; On aarch64 the serial console is the PL011 UART (`ttyAMA0'); on x86_64 it
;;; is the 16550 UART (`ttyS0').  Both match `%kernel-arguments' below.
(define %serial-tty
  (if (target-aarch64?) "ttyAMA0" "ttyS0"))

;;; On aarch64 `guix system vm' boots with no default machine and the plain
;;; `console=ttyS0' kernel argument, but QEMU's `virt' machine wires its PL011
;;; serial device to `ttyAMA0'; without this the VM boots but the console
;;; stays empty.  Select the virt machine CPU in the launcher (see the header)
;;; and point the kernel at the PL011 UART here.  On x86_64 Guix's defaults
;;; (ttyS0) already work.
(define %kernel-arguments
  (delete
   "quiet"
   (append
    (if (target-aarch64?)
        '("earlycon=pl011,0x9000000" "console=ttyAMA0")
        '())
    %default-kernel-arguments)))

;;; The base OS: the services and the serial console login, wrapped in a
;;; value so the `file-systems' field below can extend the inherited list.
(define %base-os
  (simple-operating-system
   ;; The provision one-shot requires `networking'; `guix system vm' and
   ;; `guix system container' provide the QEMU user-mode network.
   (service static-networking-service-type
            (list %qemu-static-networking))
   (service gascity-service-type (list %supervisor))
   ;; Install the Claude Code configuration before the supervisor starts.
   (simple-service 'gascity-claude-credentials
                   shepherd-root-service-type
                   (list %claude-credentials-service))
   ;; Make `user-processes' wait for the install so the supervisor (which
   ;; requires `user-processes') never starts without its credentials.
   (simple-service 'gascity-claude-credentials-order
                   user-processes-service-type
                   '(gascity-claude-credentials))
   ;; A serial console login.  Root has no password on a fresh Guix System,
   ;; so `login:' accepts `root' directly.
   (service agetty-service-type
            (agetty-configuration
             (tty %serial-tty)
             (term "vt100")
             (baud-rate "115200")))))

(operating-system
 (inherit %base-os)
 ;; `guix system vm' shares the host store but boots a tiny root image (under
 ;; 0.5 GiB free here), and Dolt's managed server refuses to start below its
 ;; 0.5 GiB free-space floor (`GC_DOLT_MIN_FREE_BYTES'), which would fail
 ;; genesis for a reason unrelated to the service.  Mount a roomy tmpfs at
 ;; the service state directory from the initrd (`needed-for-boot?') so it
 ;; exists before activation materializes the generated TOML; the demo state
 ;; is volatile anyway.  (A later mount would shadow the generated files.)
 ;; The same tmpfs caps `guix system container' too, where the state dir would
 ;; otherwise live on the (large) container root.  1 GiB proved too small:
 ;; the bead store and session history filled it, which starved Dolt
 ;; (connection refused) and stalled every agent.  4 GiB is ample and tmpfs
 ;; only charges pages that are actually written.
 (file-systems
  (cons (file-system
          (mount-point (gascity-supervisor-state-directory %supervisor))
          (device "none")
          (type "tmpfs")
          (create-mount-point? #t)
          (needed-for-boot? #t)
          (options "size=4096M"))
        (operating-system-file-systems %base-os)))
 ;; `claude-code' on the system PATH so `claude' can be run by hand from the
 ;; console; the supervisor's own `packages' field adds it to its agent PATH.
 ;; `git' is here because `gc rig add' and the bundled bead shim
 ;; (`gc-beads-bd.sh') look it up by name from the interactive shell.
 (packages (cons* claude-code git %base-packages))
 ;; The VM fallback boots the kernel directly with `-kernel', so the
 ;; bootloader is never installed.  u-boot-bootloader carries no package,
 ;; unlike the default grub-bootloader, which cannot even be built on
 ;; aarch64 hosts (grub-pc); this keeps the VM build offline and portable.
 (bootloader
  (bootloader-configuration
   (bootloader u-boot-bootloader)
   (targets '("/dev/sdX"))))
 ;; Load the virtio NIC driver before shepherd starts; on aarch64 -M virt the
 ;; NIC is virtio-net-pci.  Without this the `%qemu-static-networking' device
 ;; has no driver and the `networking' provision never comes up.
 (initrd-modules (cons* "virtio_net" %base-initrd-modules))
 (kernel-arguments %kernel-arguments))
