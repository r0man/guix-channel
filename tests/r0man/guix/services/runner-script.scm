;;; Unit tests for github-actions-runner-start-script.  The script, plus
;;; a fake runner package standing in for the actions-runner package, is
;;; lowered to the store and actually executed, so that the registration
;;; invocation, idempotency, shell quoting, and error handling are
;;; exercised for real.

(define-module (test-r0man-guix-services-runner-script)
  #:use-module (gnu packages bash)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module ((guix build utils) #:select (delete-file-recursively))
  #:use-module (r0man guix services github-actions)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64))

(define %store (open-connection))

(define (build obj)
  "Lower OBJ to the store, build it, and return its output path."
  (run-with-store %store
    (mlet %store-monad ((drv (lower-object obj)))
      (mbegin %store-monad
        (built-derivations (list drv))
        (return (derivation->output-path drv))))))

;; A stand-in for the actions-runner package: the two launchers the
;; start script invokes, replaced by scripts that record their command
;; line and the FOO environment variable to $FAKE_LOG and exit 0.
(define %fake-runner
  (computed-file
   "fake-actions-runner"
   (with-imported-modules '((guix build utils))
     #~(begin
       (use-modules (guix build utils))
       (define bash #$(file-append bash-minimal "/bin/bash"))
       (define (fake name body)
         (let ((file (string-append #$output "/bin/" name)))
           (call-with-output-file file
             (lambda (port)
               (format port "#!~a\n~a" bash body)))
           (chmod file #o555)))
       (mkdir-p (string-append #$output "/bin"))
       (fake "actions-runner-config"
             "echo config \"$@\" >> \"$FAKE_LOG\"
printf '[%s]' \"$@\" >> \"$FAKE_LOG\"
echo >> \"$FAKE_LOG\"
echo env \"$FOO\" >> \"$FAKE_LOG\"
")
       (fake "actions-runner"
             "echo run \"$@\" >> \"$FAKE_LOG\"
")))))

(define %bash (build bash-minimal))

(define (read-file file)
  (call-with-input-file file get-string-all))

(define (call-with-work-dir proc)
  "Call PROC with a fresh, empty temporary directory, deleted on exit."
  (let ((work-dir (mkdtemp! "/tmp/github-actions-runner-test-XXXXXX")))
    (dynamic-wind
      (const #t)
      (lambda () (proc work-dir))
      (lambda () (delete-file-recursively work-dir)))))

(define (build-script work-dir . kwargs)
  "Build a start script for the work directory WORK-DIR and return its
store path."
  (build
   (apply github-actions-runner-start-script
          #:package %fake-runner
          #:work-dir work-dir
          kwargs)))

(define (run-script script work-dir)
  "Run the start script SCRIPT with work directory WORK-DIR.  Its
output and the fake runner log are written into WORK-DIR.  Return #t if
the script exited with zero."
  (setenv "FAKE_LOG" (string-append work-dir "/fake.log"))
  ;; Empty PATH, as shepherd runs the script with a clean environment
  ;; on a Guix System — any bare `mkdir' in the script must fail.
  (let ((parent-path (getenv "PATH")))
    (dynamic-wind
      (lambda () (setenv "PATH" "/nonexistent"))
      (lambda ()
        (zero? (system* (string-append %bash "/bin/bash") "-c"
                        (string-append "exec \"" %bash
                                 "/bin/bash\" \"$1\" >\""
                                 work-dir "/output.log\" 2>&1")
                  script script)))
      (lambda () (setenv "PATH" parent-path)))))

(test-begin "github-actions-runner-start-script")

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "registers the runner and execs it when unconfigured"
     (let* ((script (build-script
                     work-dir
                     #:url "https://github.com/example/example"
                     ;; Include characters that are special inside a
                     ;; POSIX double-quoted string.
                     #:token "tok1en\"with$special"
                     #:name "ci-box"
                     #:labels '("linux" "x64")))
            (ok (run-script script work-dir))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok
            (string-contains
             log
             (string-append
              "config --unattended --url https://github.com/example/example"
              " --token tok1en\"with$special --name ci-box"
              " --labels linux,x64"))
            (string-contains log "run")
            #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "skips registration when a .runner file exists"
     (let* ((script (build-script
                     work-dir
                     #:url "https://github.com/example/example"
                     #:token "abc123"))
            (_ (call-with-output-file (string-append work-dir "/.runner")
                 (lambda (port) (display "{}" port))))
            (ok (run-script script work-dir))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok
            (string-contains log "run")
            (not (string-contains log "config"))
            #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "fails without registration credentials"
     (let* ((script (build-script
                     work-dir
                     #:url "https://github.com/example/example"))
            (ok (run-script script work-dir))
            (output (read-file (string-append work-dir "/output.log"))))
       (and (not ok)
            (not (file-exists? (string-append work-dir "/fake.log")))
            (string-contains output "not registered with GitHub")
            #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "reads the token from a file-like object"
     (let* ((script (build-script
                     work-dir
                     #:url "https://github.com/example/example"
                     #:token (plain-file "test-runner-token"
                                         "file-tok\n")))
            (ok (run-script script work-dir))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok
            ;; The bracket delimiters prove that the newline in the
            ;; token file was stripped and nothing else leaked in.
            (string-contains log "[--token][file-tok]")
            (not (string-contains log "[file-tok\n]"))
            #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "exports environment variables"
     (let* ((script (build-script
                     work-dir
                     #:url "https://github.com/example/example"
                     #:token "abc123"
                     #:environment-variables '("FOO=bar baz")))
            (ok (run-script script work-dir))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok (string-contains log "env bar baz") #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "passes --replace when requested"
     (let* ((script (build-script
                     work-dir
                     #:url "https://github.com/example/example"
                     #:token "abc123"
                     #:replace? #t))
            (ok (run-script script work-dir))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok (string-contains log "--replace") #t)))))

(test-end "github-actions-runner-start-script")
