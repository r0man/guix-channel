;;; Unit tests for github-actions-runner-token-mint-service-type.  The
;;; mint script is lowered to the store and actually executed, so that
;;; the registered-skip, the unreadable-PAT failure, the chown to the
;;; runner account, and the minting itself (against a mock GitHub API
;;; served on loopback) are exercised for real.

(define-module (test-r0man-guix-services-github-actions-token-mint)
  #:use-module (gnu packages bash)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module ((guix build utils) #:select (delete-file-recursively))
  #:use-module (r0man guix services github-actions)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)               ; call-with-new-thread
  #:use-module (web client)
  #:use-module (web request)                 ; request-method
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)                     ; request-uri/uri-path
  #:use-module (srfi srfi-64))

(define %store (open-connection))

(define (build obj)
  "Lower OBJ to the store, build it, and return its output path."
  (run-with-store %store
    (mlet %store-monad ((drv (lower-object obj)))
      (mbegin %store-monad
        (built-derivations (list drv))
        (return (derivation->output-path drv))))))

(define %bash (build bash-minimal))

(define (current-user-name)
  "The account name the chown in the mint script can target: the user
running the test."
  (passwd:name (getpwuid (getuid))))

(define (current-group-name)
  "The group name the chown in the mint script can target: the group
running the test."
  (group:name (getgrgid (getgid))))

(define (call-with-work-dir proc)
  "Call PROC with a fresh, empty temporary directory, deleted on exit."
  (let ((work-dir (mkdtemp! "/tmp/github-actions-runner-mint-XXXXXX")))
    (dynamic-wind
      (const #t)
      (lambda () (proc work-dir))
      (lambda () (delete-file-recursively work-dir)))))

(define (build-mint-script work-dir url pat-file)
  "Build the mint script of a token configuration pointed at the
work-directory WORK-DIR (for its .runner and token files) and return
its store path."
  (build
   (github-actions-runner-token-mint-script
    (github-actions-runner-token-configuration
     (url url)
     (pat-file pat-file)
     (token-file (string-append work-dir "/registration-token"))
     (runner-config-file (string-append work-dir "/.runner"))
     (user (current-user-name))
     (group (current-group-name))))))

(define (run-script script work-dir)
  "Run the mint script SCRIPT; its output goes to WORK-DIR.  Return #t
if the script exited with zero."
  ;; Empty PATH, as shepherd runs the script with a clean environment
  ;; on a Guix System — any bare `mkdir' or `dirname' in the script
  ;; must fail.
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

(define (read-file file)
  (call-with-input-file file get-string-all))

(define (free-port)
  "Return a TCP port that is free right now on the loopback interface."
  (let ((s (socket AF_INET SOCK_STREAM 0)))
    (bind s AF_INET INADDR_ANY 0)
    (let ((port (sockaddr:port (getsockname s))))
      (close s)
      port)))

(define (call-with-mock-api handler proc)
  "Serve HANDLER (a procedure of method, path, headers, body returning
two values: status code and JSON body) on a local HTTP server, and call
PROC with a thunk returning the requests seen so far, oldest first."
  (define requests '())
  (define port (free-port))
  (define (record method path)
    (set! requests (cons (list method path) requests))
    (values (build-response #:code (handler method path))
            "{\"token\":\"MINTED-123\"}"))
  (define (server-handler request body)
    (record (request-method request) (uri-path (request-uri request))))
  (call-with-new-thread
   (lambda ()
     (catch #t
       (lambda ()
         (run-server server-handler 'http
                     (list #:port port #:host "127.0.0.1")))
       (const #t))))
  (dynamic-wind
    (const #t)
    (lambda ()
      ;; Wait for the server to accept connections before handing the
      ;; URL to the test.  (sleep 0.05) errors in Guile core; nanosleep
      ;; comes from (ice-9 threads)... neither is guaranteed here, so
      ;; poll with a blocking read of a closed port instead.
      (let poll ()
        (or (false-if-exception
             (begin (http-request
                     (string-append "http://127.0.0.1:" (number->string port)
                                    "/poll")
                    #:method 'GET)
                    #t))
            (begin (sleep 1) (poll))))
      (proc (lambda () (reverse requests))
            ;; The mint module overrides its API base with the
            ;; GITHUB_API_BASE environment variable, so the store
            ;; program minting for github.com URLs talks to the mock
            ;; server.  Pass the bare URL; the test setenv's it.
            (string-append "http://127.0.0.1:" (number->string port))))
    (lambda ()
      (set! requests '()))))

(test-begin "github-actions-runner-token-mint")

(test-group "mint script"
  (call-with-work-dir
   (lambda (work-dir)
     (let ((pat-file (string-append work-dir "/pat")))
       (call-with-output-file pat-file (lambda (port) (display "PAT-XYZ" port)))
       (test-assert "skips minting when the runner is registered"
         (let* ((script (build-mint-script work-dir
                                           "https://github.com/example/example"
                                           pat-file))
                (_ (call-with-output-file (string-append work-dir "/.runner")
                     (lambda (port) (display "{}" port))))
                (ok (run-script script work-dir)))
           (and ok
                (not (file-exists?
                      (string-append work-dir "/registration-token")))
                (string-contains (read-file (string-append work-dir
                                                           "/output.log"))
                                 "registered; not minting")))))))

  (call-with-work-dir
   (lambda (work-dir)
     (test-assert "fails when the PAT file is not readable"
       (let* ((script (build-mint-script work-dir
                                         "https://github.com/example/example"
                                         "/does/not/exist"))
              (ok (run-script script work-dir)))
         (and (not ok)
              (string-contains (read-file (string-append work-dir
                                                         "/output.log"))
                               "not readable"))))))

  (call-with-work-dir
   (lambda (work-dir)
     (let ((pat-file (string-append work-dir "/pat")))
       (call-with-output-file pat-file
         (lambda (port) (display "PAT-XYZ" port)))
       (call-with-mock-api
        (lambda (method path)
          ;; Registration token endpoint, and 404 for anything else.
          ;; METHOD is a symbol (request-method).
          (if (and (eq? method 'POST)
                   (string-contains path "/actions/runners/registration-token"))
              201
              404))
        (lambda (seen-requests api-base)
          (setenv "GITHUB_API_BASE" api-base)
          (test-assert "mints and hands the token to the runner account"
            (let* ((script (build-mint-script
                            work-dir "https://github.com/example/example"
                            pat-file))
                   (ok (run-script script work-dir))
                   (token-file (string-append work-dir "/registration-token"))
                   (stat (stat token-file)))
              (and ok
                   (string=? "MINTED-123" (read-file token-file))
                   (= #o600 (logand #o777 (stat:mode stat)))
                   (= (getuid) (stat:uid stat)))))
          (test-assert "reuses the fresh token on the next boot"
            (let* ((before (length (seen-requests)))
                   (script (build-mint-script
                            work-dir "https://github.com/example/example"
                            pat-file))
                   (ok (run-script script work-dir)))
              (and ok (= before (length (seen-requests))))))))))))

(test-end "github-actions-runner-token-mint")