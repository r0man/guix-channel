;;; System test for github-actions-runner-service-type: boot a VM
;;; running the service without registration credentials, and verify
;;; that the user account, work directories, and Shepherd service are
;;; set up, and that the start script reports that the runner is not
;;; registered.  Deliberately no registration token is provided, so the
;;; test never reaches the network.

(define-module (test-r0man-guix-services-github-actions-system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (r0man guix services github-actions)
  #:export (%test-github-actions-runner))

(define %runner-os
  (simple-operating-system
   (service static-networking-service-type
            (list %qemu-static-networking))
   (service github-actions-runner-service-type
            (github-actions-runner-configuration
             (url "https://github.com/example/example")))))

(define (runner-test command)
  (with-imported-modules '((gnu build marionette))
    #~(begin
        (use-modules (gnu build marionette)
                     (ice-9 textual-ports)
                     (srfi srfi-64))

        (define marionette (make-marionette (list #$command)))

        (test-begin "github-actions-runner")

        (test-assert "service user account exists"
          (marionette-eval '(and (getpwnam "github-actions-runner") #t)
                           marionette))

        (test-assert "service group exists"
          (marionette-eval '(and (getgrnam "github-actions-runner") #t)
                           marionette))

        (test-assert "work directory exists"
          (marionette-eval '(file-exists? "/var/lib/github-actions-runner")
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

        (test-assert "start script reports missing registration"
          (marionette-eval
           '(let ((log "/var/log/github-actions-runner.log"))
              (let loop ((attempt 0))
                (or (and (file-exists? log)
                         (string-contains
                          (call-with-input-file log get-string-all)
                          "not registered with GitHub"))
                    (>= attempt 60)
                    (begin (sleep 1) (loop (+ attempt 1))))))
           marionette))

        (test-end)
        (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

(define (skip-derivation)
  ;; The VM test uses BIOS grub, which cannot be built on non-x86_64
  ;; hosts, so skip there instead of failing.
  (gexp->derivation
   "github-actions-runner-test"
   #~(begin
       (display "SKIP: the VM test requires an x86_64 host.\n")
       (exit 0))))

(define %test-github-actions-runner
  (system-test
   (name "github-actions-runner")
   (description
    "Boot a VM running @code{github-actions-runner-service-type} without
registration credentials, and verify that the user account, work
directory, and Shepherd service are set up, and that the runner reports
that it is not registered.")
   (value
    (if (target-x86-64?)
        (mlet* %store-monad
            ((os -> (marionette-operating-system %runner-os))
             (command (system-qemu-image/shared-store-script os
                                                             #:graphic? #f)))
          (gexp->derivation "github-actions-runner-test"
                            (runner-test command)))
        (skip-derivation)))))
