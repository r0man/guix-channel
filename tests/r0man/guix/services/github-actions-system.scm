;;; System test for github-actions-runner-service-type.  This file is
;;; self-contained: it defines the test OS, the marionette VM, and the
;;; SRFI-64 gate, so one service is one test file.
;;;
;;; The VM runs the service without registration credentials, and
;;; verifies that the user account, work directories, and Shepherd
;;; service are set up, and that the start script reports that the
;;; runner is not registered.  Deliberately no registration token is
;;; provided, so the test never reaches the network.
;;;
;;; The test boots a QEMU VM via direct kernel boot on both x86_64 and
;;; aarch64: the bootloader is a placeholder (u-boot-bootloader carries
;;; no package, so nothing is built or installed), and the QEMU
;;; machine, CPU, and console differ per architecture.

(define-module (test-r0man-guix-services-github-actions-system)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (r0man guix services github-actions)
  #:use-module (srfi srfi-1)
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
;; x86_64, Guix's defaults (ttyS0) work as-is.
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

;; Files whose contents the builder dumps into the derivation output
;; for the gate to inspect.
(define %diagnostics-files
  '("/var/log/github-actions-runner.log"
    "/var/log/messages"))

(define (marionette-test vm)
  "Return the derivation builder gexp for the marionette VM VM.  The
builder always succeeds; it writes the marionette results and the guest
diagnostics into its output for the gate to inspect."
  (with-imported-modules '((gnu build marionette))
    #~(begin
        (use-modules (gnu build marionette)
                     (ice-9 textual-ports)
                     (srfi srfi-64))

        (define marionette
          (make-marionette (cons* #$vm '#$%marionette-qemu-args)))

        (test-runner-current (system-test-runner #$output))

        (test-begin "github-actions-runner")

        (test-assert "service user account exists"
          (marionette-eval '(and (getpwnam "github-actions-runner") #t)
                           marionette))

        (test-assert "service group exists"
          (marionette-eval '(and (getgrnam "github-actions-runner") #t)
                           marionette))

        (test-assert "work directory exists"
          (marionette-eval
           '(file-exists? "/var/lib/github-actions-runner")
           marionette))

        (test-assert "_work directory exists"
          (marionette-eval
           '(file-exists? "/var/lib/github-actions-runner/_work")
           marionette))

        (test-equal "work directory owned by the service user"
          '("github-actions-runner" "github-actions-runner")
          (marionette-eval
           '(let ((st (stat "/var/lib/github-actions-runner")))
              (list (passwd:name (getpwuid (stat:uid st)))
                    (group:name (getgrgid (stat:gid st)))))
           marionette))

        (test-assert "shepherd service is registered"
          (marionette-eval
           '(zero? (system* "herd" "status" "github-actions-runner"))
           marionette))

        ;; Dump guest state into the derivation output; the gate
        ;; asserts on it.
        (call-with-output-file (string-append #$output "/diagnostics.log")
          (lambda (port)
            (write
             (marionette-eval
              '(begin
                 (use-modules (ice-9 textual-ports))
                 (map (lambda (file)
                        (list file
                              (file-exists? file)
                              (and (file-exists? file)
                                   (call-with-input-file file
                                     get-string-all))))
                      '#$%diagnostics-files))
              marionette)
             port)))

        (test-end))))

(define %test-github-actions-runner
  (system-test
   (name "github-actions-runner")
   (description
    "Boot a VM running @code{github-actions-runner-service-type} without
registration credentials, and verify that the user account, work
directory, and Shepherd service are set up, and that the runner reports
that it is not registered.")
   (value
    (mlet* %store-monad
        ((os -> (marionette-operating-system %runner-os))
         (vm -> (virtual-machine
                 (operating-system os)
                 (memory-size 1024))))
      (gexp->derivation "github-actions-runner-test"
                        (marionette-test vm))))))

(define %run-system-tests?
  (make-parameter
   (let ((value (getenv "R0MAN_RUN_SYSTEM_TESTS")))
     (and (string? value)
          (not (string-null? value))
          (not (member (string-downcase value) '("0" "no" "false")))))))

(define (run-system-test test)
  "Build TEST, a system-test record, in a fresh store connection, and
check the guest diagnostics collected by the builder.  Return #t on
success, #f on failure."
  (with-store store
    (catch #t
      (lambda ()
        (let* ((drv (run-with-store store (system-test-value test)))
               (out (derivation->output-path drv)))
          (build-derivations store (list drv))
          (let* ((diagnostics
                  (call-with-input-file (string-append out "/diagnostics.log")
                    read))
                 (entry
                  (assoc "/var/log/github-actions-runner.log" diagnostics)))
            (match entry
              ((log exists text)
               (and exists
                    (string? text)
                    (string-contains text "not registered with GitHub")))
              (_ #f)))))
      (lambda (key . rest)
        (when (memq key '(quit signal))
          (apply throw key rest))
        (format (current-error-port)
                "system test failed: ~a ~s\n" key rest)
        #f))))

(test-begin "github-actions-runner-system")

(unless (%run-system-tests?)
  (test-skip 1))

(test-assert "github-actions-runner boots and passes marionette assertions"
  (run-system-test %test-github-actions-runner))

(test-end "github-actions-runner-system")
