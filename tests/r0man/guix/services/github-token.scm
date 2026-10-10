;;; Unit tests for (r0man guix services github-token): the git credential
;;; helper, the gh wrapper and the generated git configuration.
;;;
;;; The credential helper's program text has no store references besides
;;; its token-file string, so the tests run it for real without the daemon:
;;; `gexp->approximate-sexp' reduces the program to plain Scheme, which a
;;; `guile' subprocess executes with the request on its standard input.

(define-module (test-r0man-guix-services-github-token)
  #:use-module (guix gexp)
  #:use-module (r0man guix services github-token)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64))

(define %directory
  (let ((directory (string-append (or (getenv "TMPDIR") "/tmp")
                                  "/github-token-test-"
                                  (number->string (getpid)))))
    (mkdir directory)
    directory))

(define %token-file
  (string-append %directory "/token"))

(define (write-file file content)
  (call-with-output-file file
    (lambda (port) (display content port))))

(define %program
  (string-append %directory "/helper.scm"))

;; The helper's program, reduced to plain Scheme.
(call-with-output-file %program
  (lambda (port)
    (write (gexp->approximate-sexp
            (program-file-gexp
             (github-token-credential-helper %token-file)))
           port)))

(define (run-helper action request)
  "Run the credential helper with ACTION and REQUEST on its standard input.
Return a pair of its exit code and its standard output."
  (let* ((input (string-append %directory "/request"))
         (output (string-append %directory "/answer")))
    (write-file input request)
    (let ((status (system (string-append "guile --no-auto-compile -s "
                                         %program " " action
                                         " < " input " > " output
                                         " 2>/dev/null"))))
      (cons (status:exit-val status)
            (call-with-input-file output get-string-all)))))

(define %github-request
  "protocol=https\nhost=github.com\npath=r0man/guix-channel.git\n\n")

(define (iso-8601 seconds)
  (strftime "%Y-%m-%dT%H:%M:%SZ" (gmtime seconds)))

(test-begin "github-token")

(test-group "credential helper"
  (write-file %token-file "ghs_dummyinstallationtoken\n")

  (test-equal "get for github.com answers with the trimmed token"
    (cons 0 "username=x-access-token\npassword=ghs_dummyinstallationtoken\n")
    (run-helper "get" %github-request))

  (test-equal "another host stays silent"
    (cons 0 "")
    (run-helper "get" "protocol=https\nhost=evil.example\n\n"))

  (test-equal "plain http to github.com stays silent"
    (cons 0 "")
    (run-helper "get" "protocol=http\nhost=github.com\n\n"))

  (test-equal "store does nothing"
    (cons 0 "")
    (run-helper "store" (string-append %github-request
                                        "username=x\npassword=y\n")))

  (test-equal "erase does nothing"
    (cons 0 "")
    (run-helper "erase" %github-request))

  (test-equal "a future expires_at sidecar still answers"
    (cons 0 "username=x-access-token\npassword=ghs_dummyinstallationtoken\n")
    (begin
      (write-file (string-append %token-file ".expires_at")
                  (string-append (iso-8601 (+ (current-time) 3600)) "\n"))
      (run-helper "get" %github-request)))

  (test-equal "a stale token errors out and answers nothing"
    (cons 1 "")
    (begin
      (write-file (string-append %token-file ".expires_at")
                  (iso-8601 (- (current-time) 60)))
      (run-helper "get" %github-request)))

  (test-equal "a missing token file errors out"
    (cons 1 "")
    (begin
      (delete-file (string-append %token-file ".expires_at"))
      (delete-file %token-file)
      (run-helper "get" %github-request))))

(test-group "expires_at parsing"
  (define parse
    (let ((module (make-fresh-user-module)))
      (eval '(use-modules (ice-9 rdelim) (ice-9 textual-ports)
                          (srfi srfi-1) (srfi srfi-13) (srfi srfi-19))
            module)
      (for-each (lambda (form) (eval form module))
                github-token-credential-forms)
      (eval 'github-token-parse-expires-at module)))

  (test-equal "a GitHub timestamp is UTC"
    1791462896
    (parse "2026-10-08T12:34:56Z"))

  (test-equal "fractional seconds and +00:00 are tolerated"
    1791462896
    (parse "2026-10-08T12:34:56.123+00:00"))

  (test-equal "epoch seconds are accepted"
    1791462896
    (parse "1791462896\n"))

  (test-assert "a non-UTC offset is rejected"
    (not (parse "2026-10-08T12:34:56+02:00")))

  (test-assert "garbage is rejected"
    (not (parse "soon"))))

(test-group "git config"
  (define text
    (format #f "~s"
            (gexp->approximate-sexp
             (computed-file-gexp
              (github-git-config #:name "gascity[bot]"
                                 #:email "1+gascity[bot]@users.noreply.github.com"
                                 #:token-file "/run/github-app-token/gascity/token")))))

  (test-assert "the identity is set"
    (and (string-contains text "gascity[bot]")
         (string-contains text "users.noreply.github.com")))

  (test-assert "the credential helper is scoped to https://github.com"
    (and (string-contains text "[credential \\\"https://github.com\\\"]")
         (string-contains text "github-token-credential-helper")))

  (test-assert "SSH remotes are rewritten to HTTPS"
    (and (string-contains text "insteadOf = git@github.com:")
         (string-contains text "insteadOf = ssh://git@github.com/")))

  (test-assert "push.autoSetupRemote is on"
    (string-contains text "autoSetupRemote = true"))

  (test-assert "a token file or a helper is required"
    (catch #t
      (lambda () (github-git-config #:name "n" #:email "e") #f)
      (lambda _ #t))))

(test-group "gh wrapper"
  (test-assert "the wrapper is a file-union with bin/gh"
    (file-like? (github-token-gh "/run/token" (plain-file "gh" "")))))

(test-end "github-token")

(system* "rm" "-rf" %directory)
