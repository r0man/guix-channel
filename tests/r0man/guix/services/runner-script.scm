;;; Unit tests for github-actions-runner-start-script.  The program, plus
;;; a fake runner package standing in for the actions-runner package, is
;;; lowered to the store and actually executed with an empty PATH, so
;;; that the registration invocation, idempotency, argument passing, the
;;; launchers' environment, and error handling are exercised for real.

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
       ;; The bare mkdir is what the real launcher's init block runs
       ;; before it sets a PATH of its own.
       (fake "actions-runner-config"
             "echo config \"$@\" >> \"$FAKE_LOG\"
printf '[%s]' \"$@\" >> \"$FAKE_LOG\"
echo >> \"$FAKE_LOG\"
echo env \"$FOO\" >> \"$FAKE_LOG\"
echo home \"$HOME\" >> \"$FAKE_LOG\"
mkdir -p \"$HOME/probe\" && echo mkdir-ok >> \"$FAKE_LOG\"
")
       (fake "actions-runner"
             "echo run \"$@\" >> \"$FAKE_LOG\"
exit \"${FAKE_RUN_STATUS:-0}\"
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
  "Run the start program SCRIPT with work directory WORK-DIR.  Its
output and the fake runner log are written into WORK-DIR.  Return the
exit status of the program."
  (setenv "FAKE_LOG" (string-append work-dir "/fake.log"))
  ;; Empty PATH, as shepherd runs the program with a clean environment
  ;; on a Guix System: the program, and the launchers it runs, must not
  ;; depend on the caller's PATH.  Bash only redirects the output; the
  ;; program is executed by its own file name.
  (let ((parent-path (getenv "PATH")))
    (dynamic-wind
      (lambda () (setenv "PATH" "/nonexistent"))
      (lambda ()
        (status:exit-val
         (system* (string-append %bash "/bin/bash") "-c"
                  (string-append "exec \"$1\" >\""
                                 work-dir "/output.log\" 2>&1")
                  script script)))
      (lambda () (setenv "PATH" parent-path)))))

(define (run-script-ok? script work-dir)
  "Run SCRIPT like `run-script' and return #t if it exited with zero."
  (eqv? 0 (run-script script work-dir)))

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
            (ok (run-script-ok? script work-dir))
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
            (ok (run-script-ok? script work-dir))
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
            (ok (run-script-ok? script work-dir))
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
            (ok (run-script-ok? script work-dir))
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
            (ok (run-script-ok? script work-dir))
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
            (ok (run-script-ok? script work-dir))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok (string-contains log "--replace") #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "the launchers inherit a PATH with coreutils"
     (let* ((script (build-script
                     work-dir
                     #:url "https://github.com/example/example"
                     #:token "abc123"))
            (ok (run-script-ok? script work-dir))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok
            (string-contains log (string-append "home " work-dir "\n"))
            (string-contains log "mkdir-ok")
            (file-exists? (string-append work-dir "/probe"))
            #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "ephemeral runs touch the marker and the shutdown file"
     (let* ((marker (string-append work-dir "/feedback/registered"))
            (shutdown (string-append work-dir "/shutdown"))
            (script (build-script
                     work-dir
                     #:url "https://github.com/example/example"
                     #:token "abc123"
                     #:ephemeral? #t
                     #:shutdown-file shutdown
                     #:registration-marker marker))
            ;; The runner succeeds: a failing one would wait 300
            ;; seconds before touching the shutdown file.
            (status (run-script script work-dir))
            (output (read-file (string-append work-dir "/output.log"))))
       (and (eqv? 0 status)
            (file-exists? marker)
            (file-exists? shutdown)
            (string-contains output "runner exited with status 0")
            #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "ephemeral runs return the runner's exit status"
     (let* ((script (build-script
                     work-dir
                     #:url "https://github.com/example/example"
                     #:token "abc123"
                     #:ephemeral? #t))
            (status (dynamic-wind
                      (lambda () (setenv "FAKE_RUN_STATUS" "3"))
                      (lambda () (run-script script work-dir))
                      (lambda () (unsetenv "FAKE_RUN_STATUS")))))
       (eqv? 3 status)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "defaults the work directory to XDG_DATA_HOME"
     (let* ((script (build (github-actions-runner-start-script
                            #:package %fake-runner
                            #:url "https://github.com/example/example"
                            #:token "abc123")))
            (runner-dir (string-append work-dir "/actions-runner"))
            (ok (dynamic-wind
                  (lambda () (setenv "XDG_DATA_HOME" work-dir))
                  (lambda () (run-script-ok? script work-dir))
                  (lambda () (unsetenv "XDG_DATA_HOME"))))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok
            (string-contains log (string-append "home " runner-dir "\n"))
            (eq? (quote directory) (stat:type (stat runner-dir)))
            #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "suffixes the default work directory with the id"
     (let* ((script (build (github-actions-runner-start-script
                            #:package %fake-runner
                            #:id "ci-2"
                            #:url "https://github.com/example/example"
                            #:token "abc123")))
            (runner-dir (string-append work-dir "/actions-runner-ci-2"))
            (ok (dynamic-wind
                  (lambda () (setenv "XDG_DATA_HOME" work-dir))
                  (lambda () (run-script-ok? script work-dir))
                  (lambda () (unsetenv "XDG_DATA_HOME"))))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok
            (string-contains log (string-append "home " runner-dir "\n"))
            (eq? (quote directory) (stat:type (stat runner-dir)))
            #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "derives the runner name from the host name and the id"
     (let* ((script (build-script
                     work-dir
                     #:id "ci-2"
                     #:url "https://github.com/example/example"
                     #:token "abc123"))
            (ok (run-script-ok? script work-dir))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok
            ;; The program runs on this host, so it derives the same
            ;; name the test can predict.
            (string-contains log (string-append "[--name][" (gethostname)
                                                "-ci-2]"))
            #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "an explicit name wins over the derived one"
     (let* ((script (build-script
                     work-dir
                     #:id "ci-2"
                     #:name "ci-box"
                     #:url "https://github.com/example/example"
                     #:token "abc123"))
            (ok (run-script-ok? script work-dir))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok
            (string-contains log "[--name][ci-box]")
            (not (string-contains log (gethostname)))
            #t)))))

(call-with-work-dir
 (lambda (work-dir)
   (test-assert "passes no name without an id"
     (let* ((script (build-script
                     work-dir
                     #:url "https://github.com/example/example"
                     #:token "abc123"))
            (ok (run-script-ok? script work-dir))
            (log (read-file (string-append work-dir "/fake.log"))))
       (and ok
            (not (string-contains log "--name"))
            #t)))))

(test-end "github-actions-runner-start-script")
