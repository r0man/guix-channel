;;; Run the system tests given as command-line arguments (test module
;;; files).  Each file is loaded, every `system-test' object it defines
;;; is collected, and the test derivations are built.  The derivation's
;;; build log holds the SRFI-64 output of the test; print its path so it
;;; can be inspected with `guix build --log-file' or directly.

(use-modules (gnu tests)
             (guix derivations)
             (guix monads)
             (guix store)
             (ice-9 format)
             (ice-9 match)
             (srfi srfi-1))

(define (load-system-tests file)
  "Load FILE and return every system-test object it defines."
  (let ((tests '()))
    (save-module-excursion
     (lambda ()
       (set-current-module (make-fresh-user-module))
       (primitive-load file)
       ;; 'define-module' in FILE made its own module current, so scan
       ;; that one.
       (module-for-each
        (lambda (name variable)
          (let ((value (variable-ref variable)))
            (when (system-test? value)
              (set! tests (cons value tests)))))
        (current-module))))
    tests))

(define (show-log store derivation)
  (let ((log (log-file store (derivation-file-name derivation))))
    (if log
        (format (current-output-port)
                "test log: ~a~%" log)
        (format (current-output-port)
                "test log: not found for ~a~%"
                (derivation-file-name derivation)))))

(define (run-test store test)
  "Build the derivation of TEST and return #t on success, #f on
failure."
  (format (current-output-port) "running system test '~a'...~%"
          (system-test-name test))
  (force-output)
  (catch #t
    (lambda ()
      (let ((derivation
             (run-with-store store (system-test-value test))))
        (catch #t
          (lambda ()
            (run-with-store store
              (mbegin %store-monad
                (built-derivations (list derivation)))))
          (lambda args
            (show-log store derivation)
            (apply throw args))))
      (format (current-output-port) "PASS: ~a~%"
              (system-test-name test))
      #t)
    (lambda (key . args)
      (format (current-error-port) "FAIL: ~a~%exception: ~a ~a~%"
              (system-test-name test) key args)
      #f)))

(define (main args)
  (let* ((files (cdr args))
         (tests (append-map load-system-tests files)))
    (if (null? tests)
        (begin
          (format (current-error-port)
                  "no system tests found in:~{ ~a~}~%" files)
          (exit 1))
        (let ((results
               (with-store store
                 (set-build-options store #:keep-going? #t)
                 (map (lambda (test) (run-test store test))
                      tests))))
          (exit (if (every identity results) 0 1))))))

(main (command-line))
