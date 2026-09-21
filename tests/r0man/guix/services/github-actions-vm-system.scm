;;; System tests for the VM-backed GitHub Actions runner service.
;;;
;;; Two tests:
;;;
;;; 1. github-actions-runner-vm-guest -- boots the *inner* runner VM
;;;    operating system in a marionette VM and asserts that dockerd,
;;;    the scratch disk service, the shutdown watcher, and the runner
;;;    account come up, and that the runner (running without
;;;    registration credentials) reports that it is not registered.
;;;
;;; 2. github-actions-runner-vm-host -- builds the derivations the host
;;;    service produces (the QEMU boot scripts of the pool, the token
;;;    minting program, and the runner removal program) and asserts
;;;    their contents.
;;;
;;; As with the (gnu tests ...) modules, the files define `system-test'
;;; objects; build and inspect a failing test with:
;;;
;;;   guix build --log-file $(guix build -e '(@ (tests r0man guix services github-actions-vm-system) %test-github-actions-runner-vm-guest)')

(define-module (tests r0man guix services github-actions-vm-system)
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
  #:use-module (r0man guix services github-actions-vm)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:export (%test-github-actions-runner-vm-guest
            %test-github-actions-runner-vm-host))

(define %supported-systems '("x86_64-linux" "aarch64-linux"))

;; On aarch64 there is no default machine Guix's serial console works
;; with: use the virt machine and a CPU that advertises every feature
;; the active accelerator provides.  On x86_64, Guix's defaults work.
(define %marionette-qemu-args
  (if (target-aarch64?) '("-M" "virt" "-cpu" "max") '()))

(define (skip-derivation name)
  (gexp->derivation
   name
   #~(begin
       (display "SKIP: the VM test requires an x86_64 or aarch64 host.\n")
       (exit 0))))

;;;
;;; Test 1: boot the inner runner VM operating system.
;;;

(define (run-github-actions-runner-vm-guest-test)
  "Boot the inner runner VM operating system in a marionette VM and
check that its services come up.  No registration credentials are
available in the test (the seed directory is empty), so the runner
reports that it is not registered."
  (define config
    (github-actions-runner-vm-configuration
     (url "https://github.com/example/example")))
  (define os
    (marionette-operating-system
     (operating-system-with-gc-roots
      (github-actions-runner-vm-operating-system config)
      ;; Keep the runner package (a binary tarball) alive so that it is
      ;; not garbage-collected while the test VM boots.
      (list (github-actions-runner-vm-configuration-runner-package
             config)))))
  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 2048)))

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

          (test-begin "github-actions-runner-vm-guest")

          (test-assert "dockerd service is running"
            (marionette-eval
             '(begin
                ;; dockerd needs containerd, dbus, elogind, and the
                ;; scratch disk first; give it a few minutes.
                (let loop ((attempt 0))
                  (or (zero? (system* "herd" "status" "dockerd"))
                      (>= attempt 60)
                      (begin (sleep 5) (loop (+ attempt 1))))))
             marionette))

          (test-assert "runner user is in the docker group"
            (marionette-eval
             '(zero?
               (system* "/bin/sh" "-c"
                        "id -nG github-actions-runner | grep -qw docker"))
             marionette))

          (test-assert "scratch directory exists"
            (marionette-eval
             '(file-exists? "/scratch")
             marionette))

          (test-assert "shutdown watcher service is running"
            (marionette-eval
             '(zero? (system* "herd" "status" "github-actions-vm-shutdown"))
             marionette))

          (test-assert "runner service has exited (it is not registered)"
            (marionette-eval
             '(let loop ((attempt 0))
                (or (not (zero? (system* "herd" "status"
                                         "github-actions-runner")))
                    (>= attempt 120)
                    (begin (sleep 5) (loop (+ attempt 1)))))
             marionette))

          (test-assert "start script reports missing registration"
            (let ((result
                   (marionette-eval
                    ;; This code runs in the guest REPL, so it must
                    ;; import its own modules.
                    '(begin
                       (use-modules (ice-9 textual-ports))
                       (let ((log "/var/log/github-actions-runner.log"))
                         (let loop ((attempt 0))
                           (or (and (file-exists? log)
                                    (string-contains
                                     (call-with-input-file log
                                       get-string-all)
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
  (gexp->derivation "github-actions-runner-vm-guest-test" test))

;;;
;;; Test 2: build the derivations of the host service.
;;;

(define (run-github-actions-runner-vm-host-test)
  "Build the QEMU boot script of each pool instance plus the store
programs of the credential lifecycle, and assert their contents."
  (define config
    (github-actions-runner-vm-configuration
     (url "https://github.com/example/example")))
  (define boot-1
    (github-actions-runner-vm-boot-script config #:index 1))
  (define boot-2
    (github-actions-runner-vm-boot-script config #:index 2))
  (define mint (github-actions-runner-vm-mint-program))
  (define remove (github-actions-vm-remove-runner-program))
  (define test
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (ice-9 textual-ports)
                       (srfi srfi-64))

          (define (read-file file)
            (call-with-input-file file get-string-all))

          (test-runner-current (system-test-runner #$output))

          (test-begin "github-actions-runner-vm-host")

          (test-assert "boot script of instance 1 contains the scratch disk"
            (string-contains (read-file #$boot-1)
                             "file=/var/lib/github-actions-runner-vm/1/scratch.qcow2,format=qcow2,if=virtio"))

          (test-assert "boot script of instance 2 contains the scratch disk"
            (string-contains (read-file #$boot-2)
                             "file=/var/lib/github-actions-runner-vm/2/scratch.qcow2,format=qcow2,if=virtio"))

          (test-assert "boot scripts 9p-mount the seed directory"
            (and (string-contains (read-file #$boot-1)
                                  "github-actions-runner-vm/1/seed")
                 (string-contains (read-file #$boot-2)
                                  "github-actions-runner-vm/2/seed")))

          (test-assert "boot scripts 9p-mount the feedback directory"
            (string-contains (read-file #$boot-1)
                             "github-actions-runner-vm/1/feedback"))

          (test-assert "boot script uses user-mode networking"
            (string-contains (read-file #$boot-1) "virtio-net-pci"))

          (test-assert "boot script caps the VM's cpus"
            (string-contains (read-file #$boot-1) "-smp 2"))

          (test-assert "boot script forbids reboot loops"
            (string-contains (read-file #$boot-1) "-no-reboot"))

          (test-assert "boot script maps the host store read-only"
            (string-contains (read-file #$boot-1) "path=/gnu/store"))

          (test-assert "mint program mints and stores tokens"
            (let ((program (read-file #$mint)))
              (and (string-contains program
                                    "mint-and-store-registration-token")
                   ;; Tests point the program at a mock server via the
                   ;; environment.
                   (string-contains program "GITHUB_API_BASE"))))

          (test-assert "remove program removes runners by name"
            (string-contains (read-file #$remove)
                             "remove-github-actions-runner"))

          (test-end))))
  (gexp->derivation "github-actions-runner-vm-host-test" test))

;;;
;;; system-test objects.
;;;

(define %test-github-actions-runner-vm-guest
  (system-test
   (name "github-actions-runner-vm-guest")
   (description
    "Boot the runner VM operating system and verify that dockerd, the
scratch disk service, the shutdown watcher, and the runner account are
set up, and that the runner reports that it is not registered.")
   (value
    (if (member (%current-system) %supported-systems)
        (run-github-actions-runner-vm-guest-test)
        (skip-derivation "github-actions-runner-vm-guest-test")))))

(define %test-github-actions-runner-vm-host
  (system-test
   (name "github-actions-runner-vm-host")
   (description
    "Build the QEMU boot scripts of the runner VM pool and the token
lifecycle store programs, and verify their contents.")
   (value
    (if (member (%current-system) %supported-systems)
        (run-github-actions-runner-vm-host-test)
        (skip-derivation "github-actions-runner-vm-host-test")))))
