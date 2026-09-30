;;; System test for the gascity-service-type, following the conventions of
;;; the (gnu tests ...) modules of upstream Guix: the file defines the test
;;; OS and exports a `system-test' object, whose value is the marionette
;;; derivation.  The derivation runs the SRFI-64 assertions via
;;; `system-test-runner', which exits non-zero on failure, so the build log
;;; holds the test output and the derivation only succeeds when all
;;; assertions pass.  Inspect the log of a failing test with:
;;;
;;;   guix build --log-file $(guix build -e '(@ (tests r0man guix services gascity-system) %test-gascity-system)')
;;;
;;; The VM boots the kernel directly (-kernel) on both x86_64 and aarch64:
;;; the bootloader is a placeholder (u-boot-bootloader carries no package, so
;;; nothing is built or installed on either architecture; grub-pc, by
;;; contrast, cannot be built on aarch64 hosts), and the QEMU machine, CPU,
;;; and console differ per architecture.
;;;
;;; The test boots a system with two supervisor instances, each with its own
;;; gc-home and port and one trivial city.  `install-packs?' is #f, so the
;;; provisioning one-shot never reaches the network.  It asserts that the
;;; paired `gascity-supervisor-<id>' Shepherd services run, that each
;;; instance's `cities.toml' lists only its own city, and that the two
;;; instances' control socket and lock paths differ (§16.2).

(define-module (tests r0man guix services gascity-system)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (r0man guix services gascity)
  #:use-module (srfi srfi-64)
  #:export (%test-gascity-system))

;; The VM boots the kernel directly via -kernel, so the bootloader is never
;; installed.  u-boot-bootloader is used as a placeholder because it carries
;; no package: there is nothing to build on any architecture (grub, by
;; contrast, cannot be built for aarch64 hosts).
(define %bootloader
  (bootloader-configuration
   (bootloader u-boot-bootloader)
   (targets '("/dev/sdX"))))

;; On aarch64 there is no default machine Guix's serial console works with:
;; use the virt machine, a CPU that advertises every feature the active
;; accelerator provides, and the PL011 UART (ttyAMA0).  On x86_64, Guix's
;; defaults (ttyS0) work as-is.  net.ifnames=0 makes the NIC appear as eth0,
;; which %qemu-static-networking configures.
(define %marionette-qemu-args
  (if (target-aarch64?) '("-M" "virt" "-cpu" "max") '()))

(define %kernel-arguments
  (delete
   "quiet"
   (append
    (if (target-aarch64?)
        '("earlycon=pl011,0x9000000" "console=ttyAMA0")
        '())
    '("net.ifnames=0")
    %default-kernel-arguments)))

;; Two supervisor instances, each with its own gc-home, port and one trivial
;; city.  The cities only need genesis and registration; no pack is fetched
;; (install-packs? #f), so provisioning stays on the local machine.
(define %gascity-os
  (operating-system
    (inherit
     (simple-operating-system
      (service static-networking-service-type
               (list %qemu-static-networking))
      (service gascity-service-type
               (list (gascity-supervisor-configuration
                      (id "a")
                      (state-directory "/var/lib/gascity/a")
                      (gc-home "/var/lib/gascity/a/.gc")
                      (port 8372)
                      (cities (list (gascity-city-configuration
                                     (name "alpha")
                                     (genesis? #t)
                                     (install-packs? #f)
                                     (register? #t)))))
                     (gascity-supervisor-configuration
                      (id "b")
                      (state-directory "/var/lib/gascity/b")
                      (gc-home "/var/lib/gascity/b/.gc")
                      (port 8373)
                      (cities (list (gascity-city-configuration
                                     (name "beta")
                                     (genesis? #t)
                                     (install-packs? #f)
                                     (register? #t)))))))))
    ;; Make sure the virtio NIC driver is loaded before shepherd starts the
    ;; networking service; on aarch64 -M virt the NIC is virtio-net-pci.
    (initrd-modules (cons* "virtio_net" %base-initrd-modules))
    (bootloader %bootloader)
    (kernel-arguments %kernel-arguments)))

(define (run-gascity-system-test)
  "Run tests in %gascity-os, which runs two Gas City supervisor instances."
  (define os
    (marionette-operating-system %gascity-os))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 1024)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 format)
                       (ice-9 textual-ports)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (cons* #$vm '#$%marionette-qemu-args)))

          (define (eventually predicate)
            ;; The supervisor starts only after its paired provision one-shot
            ;; has run, so give shepherd a bounded window to converge.
            (let loop ((attempt 0))
              (or (predicate)
                  (>= attempt 60)
                  (begin (sleep 1) (loop (+ attempt 1))))))

          (test-runner-current (system-test-runner #$output))

          (test-begin "gascity-system")

          (test-assert "the gascity account exists"
            (marionette-eval '(zero? (system* "id" "gascity")) marionette))

          (test-assert "both supervisor services are running"
            (eventually
             (lambda ()
               (marionette-eval
                '(begin
                   (use-modules (ice-9 popen) (ice-9 textual-ports))
                   (define (running? name)
                     (let* ((port (open-input-pipe
                                   (string-append "herd status " name)))
                            (text (get-string-all port)))
                       (close-pipe port)
                       (string-contains text "running")))
                   (and (running? "gascity-supervisor-a")
                        (running? "gascity-supervisor-b")))
                marionette))))

          (test-assert "state directories are owned by the gascity account"
            (marionette-eval
             '(let ((uid (passwd:uid (getpwnam "gascity"))))
                (and (= uid (stat:uid (stat "/var/lib/gascity/a")))
                     (= uid (stat:uid (stat "/var/lib/gascity/b")))))
             marionette))

          (test-assert "each registry lists only its own city"
            (eventually
             (lambda ()
               (marionette-eval
                '(let ((a #f) (b #f))
                   (false-if-exception
                    (set! a (call-with-input-file
                                "/var/lib/gascity/a/.gc/cities.toml"
                              get-string-all)))
                   (false-if-exception
                    (set! b (call-with-input-file
                                "/var/lib/gascity/b/.gc/cities.toml"
                              get-string-all)))
                   (and a b
                        (string-contains a "alpha")
                        (not (string-contains a "beta"))
                        (string-contains b "beta")
                        (not (string-contains b "alpha"))))
                marionette))))

          (test-assert "the two supervisors' control sockets and locks differ"
            (eventually
             (lambda ()
               (marionette-eval
                '(let ((sock-a "/var/lib/gascity/a/.gc/supervisor.sock")
                       (sock-b "/var/lib/gascity/b/.gc/supervisor.sock")
                       (lock-a "/var/lib/gascity/a/.gc/supervisor.lock")
                       (lock-b "/var/lib/gascity/b/.gc/supervisor.lock"))
                   (and (file-exists? sock-a)
                        (file-exists? sock-b)
                        (file-exists? lock-a)
                        (file-exists? lock-b)
                        (not (string=? sock-a sock-b))
                        (not (string=? lock-a lock-b))))
                marionette))))

          (test-end))))
  (gexp->derivation "gascity-system-test" test))

;; The VM test needs the kernel and emulator of one of these systems; skip
;; elsewhere instead of failing.
(define %supported-systems '("x86_64-linux" "aarch64-linux"))

(define (skip-derivation)
  (gexp->derivation
   "gascity-system-test"
   #~(begin
       (display "SKIP: the VM test requires an x86_64 or aarch64 host.\n")
       (exit 0))))

(define %test-gascity-system
  (system-test
   (name "gascity-system")
   (description
    "Boot a VM running two Gas City supervisor instances, each with its own
gc-home, port and trivial city, and verify that both Shepherd services run,
that each registry lists only its own city, and that the instances' control
socket and lock paths differ.")
   (value
    (if (member (%current-system) %supported-systems)
        (run-gascity-system-test)
        (skip-derivation)))))
