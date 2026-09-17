;;; System test for the github-actions-runner service type, following
;;; the conventions of the (gnu tests ...) modules of upstream Guix:
;;; the file defines the test OS and exports a `system-test' object,
;;; whose value is the marionette derivation.  The derivation runs the
;;; SRFI-64 assertions via `system-test-runner', which exits non-zero
;;; on failure, so the build log holds the test output and the
;;; derivation only succeeds when all assertions pass.  Inspect the
;;; log of a failing test with:
;;;
;;;   guix build --log-file $(guix build -e '(@ (tests r0man guix services github-actions-system) %test-github-actions-runner)')
;;;
;;; The VM boots the kernel directly (-kernel) on both x86_64 and
;;; aarch64: the bootloader is a placeholder (u-boot-bootloader carries
;;; no package, so nothing is built or installed on either
;;; architecture; grub-pc, by contrast, cannot be built on aarch64
;;; hosts), and the QEMU machine, CPU, and console differ per
;;; architecture.  The runner service is configured without
;;; registration credentials, so the test never reaches the network.

(define-module (tests r0man guix services github-actions-system)
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
  #:use-module (r0man guix services github-actions)
  #:use-module (srfi srfi-64)
  #:export (%test-github-actions-runner))

;; The VM boots the kernel directly via -kernel, so the bootloader is
;; never installed.  u-boot-bootloader is used as a placeholder because
;; it carries no package: there is nothing to build on any architecture
;; (grub, by contrast, cannot be built for aarch64 hosts).
(define %bootloader
  (bootloader-configuration
   (bootloader u-boot-bootloader)
   (targets '("/dev/sdX"))))

;; On aarch64 there is no default machine Guix's serial console works
;; with: use the virt machine, a CPU that advertises every feature the
;; active accelerator provides, and the PL011 UART (ttyAMA0).  On
;; x86_64, Guix's defaults (ttyS0) work as-is.  net.ifnames=0 makes the
;; NIC appear as eth0, which %qemu-static-networking configures.
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

(define %runner-os
  (operating-system
    (inherit
     (simple-operating-system
      (service static-networking-service-type
               (list %qemu-static-networking))
      (service github-actions-runner-service-type
               (github-actions-runner-configuration
                (url "https://github.com/example/example")))))
    ;; Make sure the virtio NIC driver is loaded before shepherd starts
    ;; the networking service; on aarch64 -M virt the NIC is
    ;; virtio-net-pci.
    (initrd-modules (cons* "virtio_net" %base-initrd-modules))
    (bootloader %bootloader)
    (kernel-arguments %kernel-arguments)))

(define (run-github-actions-runner-test)
  "Run tests in %runner-os, which has the github-actions-runner
service running without registration credentials."
  (define os
    (marionette-operating-system %runner-os))

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

          (test-runner-current (system-test-runner #$output))

          (test-begin "github-actions-runner")

          (test-assert "user account exists"
            (marionette-eval '(zero? (system* "id" "github-actions-runner"))
                             marionette))

          (test-assert "work directory exists"
            (marionette-eval
             '(file-exists? "/var/lib/github-actions-runner")
             marionette))

          (test-assert "_work directory exists"
            (marionette-eval
             '(file-exists? "/var/lib/github-actions-runner/_work")
             marionette))

          (test-assert "shepherd service is running"
            (marionette-eval
             '(zero? (system* "herd" "status" "github-actions-runner"))
             marionette))

          (test-assert "start script reports missing registration"
            (let ((result
                   (marionette-eval
                    ;; Note: this code runs in the guest REPL, so it
                    ;; must import its own modules.
                    '(begin
                       (use-modules (ice-9 textual-ports))
                       (let ((log "/var/log/github-actions-runner.log"))
                         (let loop ((attempt 0))
                           (or (and (file-exists? log)
                                    (string-contains
                                     (call-with-input-file log get-string-all)
                                     "not registered with GitHub"))
                               (>= attempt 60)
                               (begin (sleep 1) (loop (+ attempt 1)))))))
                    marionette)))
              (unless result
                (format (current-error-port)
                        "guest log: ~s~%"
                        (marionette-eval
                         '(begin
                            (use-modules (ice-9 textual-ports))
                            (false-if-exception
                             (call-with-input-file
                                 "/var/log/github-actions-runner.log"
                               get-string-all)))
                         marionette)))
              result))

          (test-end))))
  (gexp->derivation "github-actions-runner-test" test))

;; The VM test needs the kernel and emulator of one of these systems;
;; skip elsewhere instead of failing.
(define %supported-systems '("x86_64-linux" "aarch64-linux"))

(define (skip-derivation)
  (gexp->derivation
   "github-actions-runner-test"
   #~(begin
       (display "SKIP: the VM test requires an x86_64 or aarch64 host.\n")
       (exit 0))))

(define %test-github-actions-runner
  (system-test
   (name "github-actions-runner")
   (description
    "Boot a VM running the GitHub Actions runner service without
registration credentials, and verify that the user account, work
directory, and Shepherd service are set up, and that the runner reports
that it is not registered.")
   (value
    (if (member (%current-system) %supported-systems)
        (run-github-actions-runner-test)
        (skip-derivation)))))
