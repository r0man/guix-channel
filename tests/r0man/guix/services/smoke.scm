;;; Smoke test: load every service module in the channel and assert one
;;; known export is a `service-type'.  This catches typos, broken
;;; imports, and unused-binding regressions across the whole channel in
;;; one pass — far cheaper than a real `guix system' dry run.

(define-module (test-r0man-guix-services-smoke)
  #:use-module (gnu services)
  #:use-module (srfi srfi-64))

(test-begin "r0man-guix-services-smoke")

(define-syntax-rule (test-load-service name module export)
  (test-assert name
    (let ((mod (resolve-interface 'module)))
      (service-type? (module-ref mod 'export)))))

(test-load-service "github-actions-runner"
                   (r0man guix services github-actions)
                   github-actions-runner-service-type)

(test-load-service "home-github-actions-runner"
                   (r0man guix home services github-actions)
                   home-github-actions-runner-service-type)

(test-end "r0man-guix-services-smoke")
