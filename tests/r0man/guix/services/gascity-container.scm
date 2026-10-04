;;; A test-only operating system for the machine-wide Gas City container
;;; smoke test.
;;;
;;; This is deliberately NOT an importable module: `scripts/gascity-container-smoke-test
;;; system' feeds the file straight to `guix system container FILE', so the
;;; last expression is the `operating-system' value, exactly like
;;; examples/gascity/system.scm.  Keeping the assertions out of the example
;;; keeps the shipped example minimal and free of test scaffolding.
;;;
;;; The OS mirrors examples/gascity/system.scm (one supervisor, one trivial
;;; "swarm" city, `install-packs? #f') but makes two container-specific
;;; changes and adds a one-shot assertion service:
;;;
;;;   - the example's `%qemu-static-networking' is dropped.  `guix system
;;;     container' runs the OS in a network namespace with only a down
;;;     loopback device and no QEMU user-mode `eth0', so a service that tries
;;;     to configure `eth0' would fail and leave the `networking' provision
;;;     unmet.  `%base-services' already brings loopback up
;;;     (`%loopback-static-networking'); we only add a no-op service that
;;;     provides the `networking' name the provision one-shot requires, so the
;;;     boot never blocks on a missing interface and needs no network access;
;;;
;;;   - a one-shot `gascity-container-check' Shepherd service runs an
;;;     assertion program after the provision one-shot and the supervisor.
;;;     The program waits (bounded) for the supervisor to come up and then
;;;     checks the full lifecycle: the activation-materialized `city.toml' and
;;;     `pack.toml', the provision one-shot's genesis (`gc init'), site binding
;;;     (`.gc/site.toml') and registry (`cities.toml'), the running
;;;     `gc supervisor run', and that the state is owned by the `gascity'
;;;     account.  It finally runs `gc config show --validate'.  It writes
;;;     `PASS' or `FAIL: reason' plus a `done' marker into /shared, a host
;;;     directory bind-mounted read/write with `--share=SPEC'.  The writes are
;;;     atomic (temp file then rename) so the host never observes a partial
;;;     file.
;;;
;;; The host side (scripts/gascity-container-smoke-test) polls /shared/done,
;;; kills the container, and maps the result to its own exit status.

(use-modules (gnu)
             (gnu bootloader)
             (gnu bootloader u-boot)
             (gnu packages admin)
             (gnu services)
             (gnu services shepherd)
             (gnu system)
             (gnu system file-systems)
             (gnu system linux-initrd)
             (gnu tests)
             (guix gexp)
             (guix utils)
             (r0man guix services gascity)
             (r0man guix toml)
             (srfi srfi-1))

;;; The single trivial city, mirroring examples/gascity/system.scm.
(define %city
  (gascity-city-configuration
   (name "swarm")
   (install-packs? #f)
   (workspace (gascity-workspace-configuration
               (name "swarm")
               (provider "claude")))
   (providers
    (list (cons "claude"
                (gascity-provider-configuration
                 (base "builtin:claude")))))
   (daemon (gascity-daemon-configuration
            (patrol-interval "30s")
            (max-restarts 5)))
   (pack (gascity-pack-configuration
          (name "swarm")
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
   (extra-config
    (list (toml-table 'mail
                      (toml-field 'provider "smtp")
                      (toml-field 'retention_ttl "24h"))
          (toml-table 'api
                      (toml-field 'bind "127.0.0.1"))))))

;;; Normalize the service value once so the derived paths (state directory,
;;; gc-home, city directory, log files) match exactly what the service uses.
(define %supervisor
  (car
   (gascity-supervisor-configurations
    (list (gascity-supervisor-configuration
           (dolt-user-name "Gas City")
           (dolt-user-email "gascity@example.com")
           (cities (list %city)))))))

(define %state-directory (gascity-supervisor-state-directory %supervisor))
(define %gc-home (gascity-supervisor-gc-home %supervisor))
(define %city-directory (gascity-city-directory %supervisor %city))
(define %log-directory (gascity-supervisor-log-directory %supervisor))
(define %supervisor-service-name
  (symbol->string (gascity-supervisor-shepherd-service-name %supervisor)))
(define %gc
  (file-append (gascity-supervisor-configuration-package %supervisor) "/bin/gc"))
(define %herd (file-append shepherd "/bin/herd"))

;;; On aarch64 `guix system vm' boots with no default machine and the plain
;;; `console=ttyS0' kernel argument, but QEMU's `virt' machine wires its PL011
;;; serial device to `ttyAMA0'; without this the VM boots but the console
;;; stays empty.  Mirror tests/r0man/guix/services/gascity-system.scm: select
;;; the virt machine CPU in the launcher (the script) and point the kernel at
;;; the PL011 UART here.  On x86_64 Guix's defaults (ttyS0) already work.
(define %kernel-arguments
  (delete
   "quiet"
   (append
    (if (target-aarch64?)
        '("earlycon=pl011,0x9000000" "console=ttyAMA0")
        '())
    %default-kernel-arguments)))

;;; Satisfy the `networking' name the gascity provision one-shot requires,
;;; without configuring an interface (loopback is already brought up by
;;; %base-services).
(define %networking-service
  (shepherd-service
   (documentation
    "Provide the 'networking' provision in a container without an interface.")
   (provision '(networking))
   (start #~(lambda () #t))
   (stop #~(lambda (_) #f))
   (respawn? #f)))

;;; The assertion program.  It runs as root inside the container.
(define %check-program
  (program-file
   "gascity-container-check"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 popen)
                      (ice-9 textual-ports)
                      (srfi srfi-1)
                      (srfi srfi-13))

         (define user "gascity")
         (define state-directory #$%state-directory)
         (define gc-home #$%gc-home)
         (define city-directory #$%city-directory)
         (define gc #$%gc)
         (define herd #$%herd)
         (define supervisor-service #$%supervisor-service-name)
         (define supervisor-log #$(gascity-supervisor-log-file %supervisor))
         (define provision-log
           (string-append #$%log-directory "/supervisor-provision.log"))
         (define dolt-log
           (string-append city-directory
                          "/.gc/runtime/packs/dolt/dolt.log"))

         (define shared "/shared")
         (define result-file (string-append shared "/result"))
         (define done-file (string-append shared "/done"))
         (define started-file (string-append shared "/started"))
         (define progress-file (string-append shared "/progress.log"))
         (define supervisor-socket
           ;; gc puts the control socket at `<gc-home>/gc/supervisor.sock';
           ;; the provision program still probes the legacy
           ;; `<gc-home>/supervisor.sock' too, so accept either.
           (string-append gc-home "/gc/supervisor.sock"))
         (define supervisor-socket-legacy
           (string-append gc-home "/supervisor.sock"))
         (define (live-supervisor-socket)
           (cond ((file-exists? supervisor-socket) supervisor-socket)
                 ((file-exists? supervisor-socket-legacy)
                  supervisor-socket-legacy)
                 (else #f)))
         (define registry (string-append gc-home "/cities.toml"))
         (define city-toml (string-append city-directory "/city.toml"))
         (define pack-toml (string-append city-directory "/pack.toml"))
         (define site-toml (string-append city-directory "/.gc/site.toml"))

         (define (append-progress text)
           ;; Append to /shared/progress.log so a failing run is diagnosable
           ;; from the host even when the VM console produced nothing.
           (catch #t
             (lambda ()
               (let ((port (open-file progress-file "a")))
                 (display text port)
                 (force-output port)
                 (close-port port)))
             (lambda _ #f)))

         (define (log fmt . args)
           (let ((text (apply format #f fmt args)))
             (display text)
             (newline)
             (force-output)
             (append-progress (string-append text "\n"))))

         (define (log-file file)
           ;; Append a file's full contents to the progress log, labelled.
           (let ((text (slurp file)))
             (when text
               (log "----- ~a -----" file)
               (append-progress text)
               (append-progress "\n"))))

         (define (slurp file)
           (false-if-exception
            (call-with-input-file file get-string-all)))

         (define (write-atomically file content)
           ;; Write a sibling temp file and rename it into place so a reader
           ;; never observes a partial file.
           (let ((temp (string-append file ".tmp")))
             (call-with-output-file temp
               (lambda (port)
                 (display content port)
                 (force-output port)))
             (rename-file temp file)))

         (define (finish status)
           (log "verdict: ~a" status)
           (write-atomically result-file (string-append status "\n"))
           (write-atomically done-file "done\n")
           (exit 0))

         (define (dump-diagnostics)
           (for-each log-file
                     (list provision-log supervisor-log dolt-log))
           (dump-logs (string-append city-directory "/.gc"))
           ;; COPY the runtime tree to /shared...
           (catch #t
             (lambda ()
               (let ((runtime (string-append city-directory "/.gc/runtime"))
                     (destination (string-append shared "/diagnostics/runtime")))
                 (when (file-exists? runtime)
                   (copy-recursively runtime destination))))
             (lambda _ #f)))

         (define (provision-failed?)
           (let ((text (slurp provision-log)))
             (and text (string-contains text "gc init failed"))))

         (define (dump-logs root)
           ;; Dump every `*.log' under ROOT so a failed `gc init' is
           ;; diagnosable without the VM console or the guest filesystem.
           (catch #t
             (lambda ()
               (for-each (lambda (file)
                           (when (string-suffix? ".log" file)
                             (log-file file)))
                         (find-files root)))
             (lambda _ #f)))

         (define (service-running? name)
           (let* ((port (open-input-pipe
                         (string-append herd " status " name)))
                  (text (get-string-all port)))
             (close-pipe port)
             (and text
                  (or (string-contains text "It is running")
                      (string-contains text "It is started")
                      (string-contains text "running")))))

         (catch #t
           (lambda ()
             (log "check service started; GC_HOME=~a city=~a"
                  gc-home city-directory)
             (write-atomically started-file "started\n")
             (log "waiting (up to 300s) for supervisor.sock and cities.toml...")
             (let loop ((remaining 300))
               (cond
                ((and (live-supervisor-socket)
                      (file-exists? registry))
                 (log "supervisor socket (~a) and cities.toml (~a) are present"
                      (live-supervisor-socket) registry))
                ((provision-failed?)
                 (log "the provision one-shot failed; stopping early")
                 (dump-diagnostics)
                 (finish "FAIL: the provision one-shot failed (see provision log)"))
                ((<= remaining 0)
                 (log "timed out: supervisor socket=~a cities.toml=~a"
                      (live-supervisor-socket)
                      (file-exists? registry))
                 (dump-diagnostics)
                 (finish "FAIL: timed out waiting for supervisor.sock and cities.toml"))
                (else (sleep 1) (loop (- remaining 1)))))
             ;; Surface the provision one-shot's output (the `gc init' result)
             ;; and the supervisor log without needing the VM console.
             (log-file provision-log)

             ;; Collect every failure instead of stopping at the first, so a
             ;; single run reports the whole picture.
             (define problems '())
             (define (expect condition message)
               (unless condition
                 (set! problems (cons message problems))))

             (define (expect-file file)
               (expect (file-exists? file) (string-append file " is missing")))

             (expect (service-running? supervisor-service)
                     "the gascity-supervisor service is not running")

             ;; Activation materialized the generated TOML.
             (expect-file city-toml)
             (expect-file pack-toml)

             ;; The provision one-shot ran genesis, wrote the site binding and
             ;; registered the city.
             (expect-file site-toml)
             (let ((registry-text (slurp registry)))
               (expect (and registry-text
                            (string-contains registry-text "[[cities]]")
                            (string-contains registry-text "swarm"))
                       "cities.toml does not list the swarm city"))

             ;; The state is owned by the gascity account.
             (let ((uid (passwd:uid (getpwnam user))))
               (for-each
                (lambda (path)
                  (expect (and (file-exists? path)
                               (= uid (stat:uid (stat path))))
                          (string-append path " is not owned by " user)))
                (list state-directory city-directory city-toml
                      registry site-toml (live-supervisor-socket))))

             ;; `gc config show --validate' accepts the generated city.
             (setenv "HOME" state-directory)
             (setenv "GC_HOME" gc-home)
             (setenv "XDG_RUNTIME_DIR" gc-home)
             (chdir city-directory)
             (let ((status (status:exit-val (system* gc "config" "show" "--validate"))))
               (log "gc config show --validate exit status: ~a" status)
               (expect (and status (zero? status))
                       "gc config show --validate failed"))

             ;; Regression (guix-channel-rso): Shepherd forgets that the
             ;; provision one-shot already ran once it exits, so stopping and
             ;; starting the supervisor re-runs it.  That must not fail on
             ;; `gc init: already initialized' (exit 2); the supervisor must
             ;; come back within the same boot, and `herd restart' must work.
             (define (herd-ok? . args)
               (let ((code (status:exit-val (apply system* herd args))))
                 (log "herd ~a exit status: ~a" (string-join args " ") code)
                 (and code (zero? code))))

             (define (wait-until-running label)
               (let wait ((remaining 60))
                 (cond
                  ((service-running? supervisor-service)
                   (log "~a: gascity-supervisor is running again" label))
                  ((<= remaining 0)
                   (expect #f (string-append label
                                             ": gascity-supervisor did not "
                                             "come back (provision one-shot "
                                             "not re-runnable?)")))
                  (else (sleep 1) (wait (- remaining 1))))))

             (expect (herd-ok? "stop" supervisor-service)
                     "herd stop gascity-supervisor failed")
             (expect (herd-ok? "start" supervisor-service)
                     "herd start gascity-supervisor failed")
             (wait-until-running "stop/start")

             (expect (herd-ok? "restart" supervisor-service)
                     "herd restart gascity-supervisor failed")
             (wait-until-running "restart")

             (if (null? problems)
                 (finish "PASS: system")
                 (begin
                   (log "problems: ~a" (string-join (reverse problems) "; "))
                   (dump-diagnostics)
                   (finish (string-append "FAIL: "
                                          (string-join (reverse problems) "; "))))))
           (lambda (key . args)
             ;; `finish' calls `exit', which raises a 'quit exception; let it
             ;; propagate instead of turning a PASS/FAIL verdict into
             ;; "exception quit".
             (if (eq? key 'quit)
                 (apply throw key args)
                 (begin
                   (dump-diagnostics)
                   (finish (format #f "FAIL: exception ~a ~s" key args))))))))))

(define %check-service
  (shepherd-service
   (documentation "Assert the full Gas City system lifecycle in a container.")
   (provision '(gascity-container-check))
   ;; Only require `user-processes': the supervisor's own dependency chain
   ;; (networking, then the provision one-shot) may fail, and this service
   ;; must still run so it can report that failure instead of leaving the
   ;; host to time out with no diagnostics.
   (requirement '(user-processes))
   (one-shot? #t)
   (start #~(lambda _
              (zero? (system* #$%check-program))))
   (stop #~(const #f))
   (respawn? #f)))

(define %base-os
  (simple-operating-system
   (service gascity-service-type (list %supervisor))
   (simple-service 'gascity-container-networking
                   shepherd-root-service-type
                   (list %networking-service))
   (simple-service 'gascity-container-check
                   shepherd-root-service-type
                   (list %check-service))))

(operating-system
 (inherit %base-os)
 ;; `guix system vm' shares the host store but boots a tiny root image (under
 ;; 0.5 GiB free here).  Dolt's managed server refuses to start below its
 ;; 0.5 GiB free-space floor (`GC_DOLT_MIN_FREE_BYTES'), which would fail
 ;; genesis for a reason unrelated to the service.  Mount a roomy tmpfs at the
 ;; service state directory from the initrd (`needed-for-boot?') so it exists
 ;; before activation materializes the generated TOML; activation still owns
 ;; it as the service user.  (A later mount would shadow the generated files;
 ;; "/dev/shm" would too, since `activate' runs before /dev/shm is mounted.)
 ;; 1 GiB was too small for the bead store plus session history; use the same
 ;; roomy limit as the interactive demo (tmpfs only charges written pages).
 (file-systems
  (cons (file-system
          (mount-point (gascity-supervisor-state-directory %supervisor))
          (device "none")
          (type "tmpfs")
          (create-mount-point? #t)
          (needed-for-boot? #t)
          (options "size=4096M"))
        (operating-system-file-systems %base-os)))
 ;; Load the virtio NIC driver before shepherd starts; on aarch64 -M virt the
 ;; NIC is virtio-net-pci (mirrors gascity-system.scm).
 (initrd-modules (cons* "virtio_net" %base-initrd-modules))
 (kernel-arguments %kernel-arguments)
 ;; The VM fallback boots the kernel directly with `-kernel', so the
 ;; bootloader is never installed.  u-boot-bootloader carries no package,
 ;; unlike the default grub-bootloader, which cannot even be built on
 ;; aarch64 hosts (grub-pc); this keeps the VM build offline and portable.
 (bootloader
  (bootloader-configuration
   (bootloader u-boot-bootloader)
   (targets '("/dev/sdX")))))
