;;; Unit tests for the VM-backed GitHub Actions runner service: the
;;; token minting machinery ((r0man guix services github-actions-vm))
;;; is exercised against a stubbed-out HTTP layer (no sockets needed),
;;; and the ephemeral start script variants are lowered to the store
;;; and checked.

(define-module (test-r0man-guix-services-github-actions-vm)
  #:use-module (gnu packages bash)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module ((guix build utils) #:select (delete-file-recursively))
  #:use-module (r0man guix services github-actions)
  #:use-module (r0man guix services github-actions-vm)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64)
  #:use-module (web response)
  #:use-module (web uri))

(define %store (open-connection))

(define (build obj)
  "Lower OBJ to the store, build it, and return its output path."
  (run-with-store %store
    (mlet %store-monad ((drv (lower-object obj)))
      (mbegin %store-monad
        (built-derivations (list drv))
        (return (derivation->output-path drv))))))

(define (call-with-temp-dir proc)
  (let ((dir (mkdtemp! "/tmp/github-actions-vm-test-XXXXXX")))
    (dynamic-wind
      (const #t)
      (lambda () (proc dir))
      (lambda () (delete-file-recursively dir)))))

;;;
;;; A stubbed HTTP layer.
;;;
;;; HANDLER is a procedure of (method path headers body) returning two
;;; values, the HTTP status code and a JSON body.  The requests seen by
;;; the stub are recorded as (method path headers body) so that tests
;;; can assert on the methods, paths, and headers the module produces.

(define (call-with-http-stub handler proc)
  "Install HANDLER as the HTTP layer of the mint module and call PROC
with a thunk returning the requests seen so far, oldest first."
  (define requests '())
  (define (fake-http uri . kwargs)
    (define (kw-ref kw)
      (or (and=> (memq kw kwargs) cadr) #f))
    (let* ((method (or (kw-ref #:method) 'GET))
           (headers (or (kw-ref #:headers) '()))
           (body (or (kw-ref #:body) "")))
      (set! requests
            (cons (list method
                        (string-append
                         (uri-path uri)
                         (if (uri-query uri)
                             (string-append "?" (uri-query uri))
                             ""))
                        headers body)
                  requests))
      (let-values (((code json) (handler method (uri-path uri) headers body)))
        (values (build-response #:code code) json))))
  (parameterize ((%http-request fake-http))
    (proc (lambda () (reverse requests)))))

;;;
;;; Start script helpers.
;;;

;; A stand-in for the runner package, referenced only via file-append
;; paths in the generated script.
(define %fake-runner-package
  (computed-file "fake-runner"
                 #~(begin (mkdir #$output)
                          (mkdir (string-append #$output "/bin")))))

(define (build-script . kwargs)
  (build (apply github-actions-runner-start-script
                #:package %fake-runner-package
                #:work-dir "/w"
                kwargs)))

(define (script-contains? script needle)
  (string-contains (call-with-input-file script get-string-all) needle))

;;;
;;; Tests.
;;;

(test-begin "github-actions-vm")

(test-group "configuration record"
  (let ((config (github-actions-runner-vm-configuration)))
    (test-assert "qemu-minimal default"
      (package? (github-actions-runner-vm-configuration-qemu config)))
    (test-equal "memory default"
      4096 (github-actions-runner-vm-configuration-memory-size config))
    (test-equal "cpus default"
      2 (github-actions-runner-vm-configuration-cpus config))
    (test-equal "parallel instances default"
      2 (github-actions-runner-vm-configuration-parallel-instances config))
    (test-equal "ephemeral default" #t
      (github-actions-runner-vm-configuration-ephemeral? config))))

(test-group "api url derivation"
  (test-equal "repository runner"
    "https://api.github.com/repos/owner/repo/actions/runners/registration-token"
    (github-api-url "https://github.com/owner/repo"
                    "/actions/runners/registration-token"))
  (test-equal "repository runner with trailing slash"
    "https://api.github.com/repos/owner/repo/actions/runners"
    (github-api-url "https://github.com/owner/repo/" "/actions/runners"))
  (test-equal "organization runner"
    "https://api.github.com/orgs/my-org/actions/runners/registration-token"
    (github-api-url "https://github.com/my-org"
                    "/actions/runners/registration-token"))
  (test-equal "organization URL with orgs prefix"
    "https://api.github.com/orgs/my-org/actions/runners"
    (github-api-url "https://github.com/orgs/my-org" "/actions/runners"))
  (test-equal "enterprise server"
    "https://ghe.example.com/api/v3/repos/owner/repo/actions/runners"
    (github-api-url "https://ghe.example.com/owner/repo" "/actions/runners")))

(test-group "mint-registration-token"
  (call-with-http-stub
   (lambda (method path headers body)
     (values 201 "{\"token\":\"MINTED-123\"}"))
   (lambda (seen-requests)
     (test-equal "returns the token from the JSON body"
       "MINTED-123"
       (mint-registration-token "PAT-XYZ" "https://github.com/owner/repo"))
     (test-assert "POSTed to the registration-token endpoint"
       (match (seen-requests)
         (((method path headers body) _ ...)
          (and (eq? method 'POST)
               (string=? path
                         "/repos/owner/repo/actions/runners/registration-token")
               (equal? (assq-ref headers 'authorization)
                       (list 'bearer (string->symbol "PAT-XYZ")))
               (equal? (assq-ref headers 'accept)
                       '((application/vnd.github+json)))))))))
  (call-with-http-stub
   (lambda (method path headers body)
     (values 403 "{\"message\":\"Forbidden\"}"))
   (lambda (seen-requests)
     (test-assert "raises on a 403 response"
       (catch #t
         (lambda ()
           (mint-registration-token "PAT-XYZ" "https://github.com/owner/repo")
           #f)
         (lambda (key . args) #t))))))

(test-group "mint-and-store-registration-token"
  (call-with-temp-dir
   (lambda (dir)
     (let ((token-file (string-append dir "/token")))
       (call-with-http-stub
        (lambda (method path headers body)
          (values 201 "{\"token\":\"MINTED-123\"}"))
        (lambda (seen-requests)
          (test-assert "writes the token with mode 0600"
            (and (mint-and-store-registration-token
                  "PAT-XYZ" "https://github.com/owner/repo" token-file)
                 (= #o600 (logand #o777 (stat:mode (stat token-file))))))
          (test-equal "token file contents"
            "MINTED-123"
            (call-with-input-file token-file get-string-all))
          (test-assert "reuses a fresh token without minting again"
            (let ((before (length (seen-requests))))
              (and (mint-and-store-registration-token
                    "PAT-XYZ" "https://github.com/owner/repo" token-file)
                   (= before (length (seen-requests))))))))))))

(test-group "remove-github-actions-runner"
  (call-with-http-stub
   (lambda (method path headers body)
     (if (eq? method 'GET)
         (values 200
                 "{\"total_count\":2,\"runners\":[{\"id\":7,\"name\":\"other\"},{\"id\":9,\"name\":\"github-actions-vm-1\"}]}")
         (values 204 "")))
   (lambda (seen-requests)
     (test-assert "removes the runner found by name"
       (remove-github-actions-runner
        "PAT-XYZ" "https://github.com/owner/repo" "github-actions-vm-1"))
     (test-assert "issued a forced DELETE for the right id"
       (match (seen-requests)
         ((_ delete _ ...)
          (and (eq? (car delete) 'DELETE)
               (string=? (cadr delete)
                         "/repos/owner/repo/actions/runners/9?force=true")))))))
  (call-with-http-stub
   (lambda (method path headers body)
     (values 200 "{\"total_count\":1,\"runners\":[{\"id\":7,\"name\":\"other\"}]}"))
   (lambda (seen-requests)
     (test-equal "returns #f when the runner is not registered"
       #f
       (remove-github-actions-runner
        "PAT-XYZ" "https://github.com/owner/repo" "github-actions-vm-1")))))

(test-group "registry cache"
  (let ((config (github-actions-runner-vm-configuration)))
    (test-equal "registry enabled by default" #t
      (github-actions-runner-vm-configuration-registry? config))
    (test-equal "registry port default" 5000
      (github-actions-runner-vm-configuration-registry-port config)))
  (let ((config (github-actions-runner-vm-configuration
                 (registry? #f))))
    (test-equal "registry can be disabled" '()
      (github-actions-runner-vm-registry-service config)))
  (test-assert "registry proxy service is generated"
    (let* ((config (github-actions-runner-vm-configuration))
           (services (github-actions-runner-vm-registry-service config)))
      (= 1 (length services)))))

(test-group "store programs"
  (test-assert "mint program mints and stores tokens"
    (let ((program (call-with-input-file
                       (build (github-actions-runner-vm-mint-program))
                     get-string-all)))
      (and (string-contains program "mint-and-store-registration-token")
           (string-contains program "read-pat-file"))))
  (test-assert "remove program removes runners by name"
    (string-contains
     (call-with-input-file (build (github-actions-vm-remove-runner-program))
                           get-string-all)
     "remove-github-actions-runner")))

(test-group "start script"
  (test-assert "ephemeral script touches the shutdown file and the marker"
    (let ((script (build-script #:ephemeral? #t
                                #:shutdown-file "/run/shut"
                                #:registration-marker "/run/mark")))
      (and (script-contains? script "SHUTDOWN_FILE=\"/run/shut\"")
           (script-contains? script "touch \"$SHUTDOWN_FILE\"")
           (script-contains? script "REGISTRATION_MARKER=\"/run/mark\"")
           (script-contains? script "touch \"$REGISTRATION_MARKER\"")
           ;; The runner is run, not exec'd.
           (script-contains? script "\n\"$RUN\"\n"))))

  (test-assert "plain script execs the runner"
    (let ((script (build-script)))
      (and (script-contains? script "exec \"$RUN\"")
           (not (script-contains? script "SHUTDOWN_FILE=\"/run/shut\"")))))

  (test-assert "token read from an absolute path"
    (let ((script (build-script #:token "/run/github-actions-seed/token")))
      (script-contains? script "/run/github-actions-seed/token"))))

(test-end)

(close-connection %store)
