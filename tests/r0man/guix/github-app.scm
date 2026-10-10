;;; Unit tests for (r0man guix github-app): the RS256 JSON Web Token (its
;;; shape, lifetime and a signature that verifies with the public half of a
;;; freshly generated key), the mandatory narrowing of installation tokens
;;; and the forbidden permissions, the installation lookup and the mint
;;; against a stubbed `%http-request', and the atomic token storage with its
;;; skip/re-mint threshold and file modes.

(define-module (test-r0man-guix-github-app)
  #:use-module ((gnutls) #:hide (base64-encode base64-decode))
  #:use-module ((guix base64) #:select (base64-decode base64url-alphabet))
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module ((guix build utils) #:select (delete-file-recursively))
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (r0man guix github-app)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (web response)
  #:use-module (web uri))

(define (call-with-work-dir proc)
  "Call PROC with a fresh, empty temporary directory, deleted on exit."
  (let ((work-dir (mkdtemp! "/tmp/github-app-test-XXXXXX")))
    (dynamic-wind
      (const #t)
      (lambda () (proc work-dir))
      (lambda () (delete-file-recursively work-dir)))))

(define %x509-key
  (generate-x509-private-key pk-algorithm/rsa 2048 '()))

(define %private-key
  (x509-private-key->private-key %x509-key '()))

(define (write-key-file directory)
  "Write the test key as a PEM file in DIRECTORY and return its name."
  (let ((file (string-append directory "/app.pem")))
    (call-with-output-file file
      (lambda (port)
        (put-string port
                    (utf8->string
                     (export-x509-private-key %x509-key
                                              x509-certificate-format/pem)))))
    file))

(define (base64url-decode string)
  "Decode the unpadded base64url STRING."
  (let ((padding (modulo (- 4 (modulo (string-length string) 4)) 4)))
    (base64-decode (string-append string (make-string padding #\=))
                   base64url-alphabet)))

(define (jwt-parts jwt)
  (string-split jwt #\.))

(define (jwt-json part)
  (json-string->scm (utf8->string (base64url-decode part))))

(define %permissions
  '((contents . "write") (pull_requests . "write") (checks . "read")))

(define (raises? thunk)
  (guard (c (#t #t))
    (thunk)
    #f))

(define (stub-http routes)
  "Return a stand-in for `http-request' answering from ROUTES, a list of
(METHOD PATH CODE BODY) entries, and recording each request as (METHOD PATH
HEADERS BODY) in the returned box (a list in a one-element vector)."
  (let ((calls (vector '())))
    (values
     (lambda* (uri #:key method headers body #:allow-other-keys)
       (let ((path (uri-path uri)))
         (vector-set! calls 0
                      (append (vector-ref calls 0)
                              (list (list method path headers body))))
         (match (find (match-lambda
                        ((m p _ _) (and (eq? m method) (string=? p path))))
                      routes)
           ((_ _ code response-body)
            (values (build-response #:code code) response-body))
           (#f
            (values (build-response #:code 404) "{}")))))
     calls)))

(test-begin "github-app")


;;;
;;; JWT.
;;;

(test-group "JWT"
  (define now 1700000000)
  (define jwt (github-app-jwt %private-key "Iv23liTEST" #:now now))

  (test-equal "three dot-separated parts"
    3 (length (jwt-parts jwt)))

  (test-equal "the header announces RS256"
    '("RS256" "JWT")
    (let ((header (jwt-json (first (jwt-parts jwt)))))
      (list (assoc-ref header "alg") (assoc-ref header "typ"))))

  (test-assert "iss is the client ID as a JSON string, iat is backdated"
    (let ((payload (jwt-json (second (jwt-parts jwt)))))
      (and (equal? "Iv23liTEST" (assoc-ref payload "iss"))
           (= (- now 60) (assoc-ref payload "iat"))
           (= (+ now 480) (assoc-ref payload "exp")))))

  (test-assert "the lifetime stays within GitHub's 10-minute maximum"
    (let ((payload (jwt-json (second (jwt-parts jwt)))))
      (<= (- (assoc-ref payload "exp") (assoc-ref payload "iat")) 600)))

  (test-assert "no base64url padding or non-URL characters"
    (string-every (lambda (c)
                    (or (char-alphabetic? c) (char-numeric? c)
                        (memv c '(#\- #\_ #\.))))
                  jwt))

  (test-assert "the signature verifies with the public key"
    (match (jwt-parts jwt)
      ((header payload signature)
       (begin
         (public-key-verify-data
          (private-key->public-key %private-key '())
          sign-algorithm/rsa-sha256
          (string->utf8 (string-append header "." payload))
          (base64url-decode signature))
         #t))))

  (test-assert "a tampered payload does not verify"
    (match (jwt-parts jwt)
      ((header payload signature)
       (raises?
        (lambda ()
          (public-key-verify-data
           (private-key->public-key %private-key '())
           sign-algorithm/rsa-sha256
           (string->utf8 (string-append header "." payload "x"))
           (base64url-decode signature)))))))

  (test-assert "a PEM key file round-trips through read-github-app-private-key"
    (call-with-work-dir
     (lambda (directory)
       (let* ((key (read-github-app-private-key (write-key-file directory)))
              (jwt (github-app-jwt key "Iv23liTEST" #:now now)))
         (match (jwt-parts jwt)
           ((header payload signature)
            (begin
              (public-key-verify-data
               (private-key->public-key %private-key '())
               sign-algorithm/rsa-sha256
               (string->utf8 (string-append header "." payload))
               (base64url-decode signature))
              #t))))))))


;;;
;;; Narrowing.
;;;

(test-group "narrowing"
  (test-assert "repositories and permissions are accepted"
    (github-app-validate-narrowing '("guix-channel") %permissions))

  (test-assert "empty repositories are rejected"
    (raises? (lambda () (github-app-validate-narrowing '() %permissions))))

  (test-assert "missing permissions are rejected"
    (raises? (lambda () (github-app-validate-narrowing '("r") '()))))

  (for-each
   (lambda (permission)
     (test-assert (format #f "~a is forbidden" permission)
       (raises? (lambda ()
                  (github-app-validate-narrowing '("r") (list permission))))))
   '((workflows . "write") (administration . "read") (secrets . "read")
     (environments . "write") (pages . "write") (actions . "write")
     ("workflows" . "read")))

  (test-assert "actions: read is allowed"
    (github-app-validate-narrowing '("r") '((actions . "read"))))

  (test-assert "the mint refuses an un-narrowed request without calling GitHub"
    (let-values (((stub calls) (stub-http '())))
      (and (parameterize ((%http-request stub))
             (raises? (lambda ()
                        (mint-installation-token "jwt" 1
                                                 #:repositories '()
                                                 #:permissions %permissions))))
           (null? (vector-ref calls 0))))))


;;;
;;; REST API.
;;;

(test-group "REST API"
  (test-equal "the installation is looked up on the repository"
    '(42 GET "/repos/r0man/guix-channel/installation")
    (let-values (((stub calls)
                  (stub-http '((GET "/repos/r0man/guix-channel/installation"
                               200 "{\"id\": 42}")))))
      (parameterize ((%http-request stub)
                     (%github-api-base "https://api.example"))
        (let ((id (github-app-installation-id "jwt" "r0man/guix-channel")))
          (match (vector-ref calls 0)
            (((method path _ _)) (list id method path)))))))

  (test-assert "a failed lookup raises without the response body"
    (let-values (((stub calls) (stub-http '())))
      (parameterize ((%http-request stub))
        (guard (c ((error? c)
                   (not (any (lambda (irritant)
                               (and (string? irritant)
                                    (string-contains irritant "{}")))
                             (exception-irritants c)))))
          (github-app-installation-id "jwt" "r0man/guix-channel")
          #f))))

  (let-values (((stub calls)
                (stub-http
                 '((POST "/app/installations/42/access_tokens" 201
                         "{\"token\": \"ghs_minted\", \"expires_at\": \"2026-10-08T12:00:00Z\"}")))))
    (define result
      (parameterize ((%http-request stub))
        (call-with-values
            (lambda ()
              (mint-installation-token "the-jwt" 42
                                       #:repositories '("guix-channel")
                                       #:permissions %permissions))
          list)))
    (define request (car (vector-ref calls 0)))

    (test-equal "the mint returns the token and its expiry"
      '("ghs_minted" "2026-10-08T12:00:00Z")
      result)

    (test-assert "the request is authenticated with the JWT and versioned"
      (match request
        ((_ _ headers _)
         (and (equal? '(bearer the-jwt) (assq-ref headers 'authorization))
              (equal? "2022-11-28" (assq-ref headers 'x-github-api-version))
              (assq-ref headers 'user-agent)))))

    (test-equal "the body narrows repositories and permissions"
      '(#("guix-channel") "write")
      (match request
        ((_ _ _ body)
         (let ((json (json-string->scm body)))
           (list (assoc-ref json "repositories")
                 (assoc-ref (assoc-ref json "permissions") "contents"))))))))


;;;
;;; Storage.
;;;

(define %expires-at "2026-10-08T12:00:00Z")
(define %expiry (parse-github-timestamp %expires-at))

(define (mint-routes token)
  `((POST "/app/installations/42/access_tokens" 201
          ,(string-append "{\"token\": \"" token
                          "\", \"expires_at\": \"" %expires-at "\"}"))
    (DELETE "/installation/token" 204 "")))

(define* (store! directory key-file #:key now (token "ghs_new")
                 (revoke-previous? #f))
  "Run the mint-and-store procedure in DIRECTORY at time NOW against a stub
minting TOKEN; return its result and the recorded calls."
  (let-values (((stub calls) (stub-http (mint-routes token))))
    (let ((result
           (parameterize ((%http-request stub))
             (mint-and-store-installation-token
              #:private-key-file key-file
              #:client-id "Iv23liTEST"
              #:installation-id 42
              #:token-directory directory
              #:reader-group (getgid)
              #:repositories '("guix-channel")
              #:permissions %permissions
              #:revoke-previous? revoke-previous?
              #:now now))))
      (values result (vector-ref calls 0)))))

(test-group "storage"
  (test-equal "timestamps parse as UTC"
    1759924800
    (parse-github-timestamp "2025-10-08T12:00:00Z"))

  (test-equal "a garbage timestamp is #f"
    #f
    (parse-github-timestamp "soon"))

  (test-equal "the fingerprint is a 12-digit hex prefix"
    "ba7816bf8f01"
    (token-fingerprint "abc"))

  (test-assert "the first run mints and writes token and sidecar 0640"
    (call-with-work-dir
     (lambda (directory)
       (let-values (((result calls)
                     (store! directory (write-key-file directory)
                             #:now (- %expiry 3600))))
         (let ((token (string-append directory "/token"))
               (sidecar (string-append directory "/token.expires_at")))
           (and (eq? 'minted result)
                (= 1 (length calls))
                (equal? "ghs_new" (call-with-input-file token get-string-all))
                (equal? (string-append %expires-at "\n")
                        (call-with-input-file sidecar get-string-all))
                (= #o640 (stat:perms (stat token)))
                (= #o640 (stat:perms (stat sidecar)))
                (= (getgid) (stat:gid (stat token)))
                (not (file-exists? (string-append token ".tmp")))))))))

  (test-assert "a token valid for more than 30 minutes is kept"
    (call-with-work-dir
     (lambda (directory)
       (let ((key-file (write-key-file directory)))
         (store! directory key-file #:now (- %expiry 3600))
         (let-values (((result calls)
                       (store! directory key-file #:now (- %expiry 1801)
                               #:token "ghs_other")))
           (and (eq? 'skipped result)
                (null? calls)
                (equal? "ghs_new"
                        (call-with-input-file (string-append directory "/token")
                          get-string-all))))))))

  (test-assert "a token expiring within 30 minutes is re-minted"
    (call-with-work-dir
     (lambda (directory)
       (let ((key-file (write-key-file directory)))
         (store! directory key-file #:now (- %expiry 3600))
         (let-values (((result calls)
                       (store! directory key-file #:now (- %expiry 1799)
                               #:token "ghs_other")))
           (and (eq? 'minted result)
                (equal? "ghs_other"
                        (call-with-input-file (string-append directory "/token")
                          get-string-all))))))))

  (test-assert "a leftover token.tmp does not block the write"
    (call-with-work-dir
     (lambda (directory)
       (call-with-output-file (string-append directory "/token.tmp")
         (lambda (port) (display "stale" port)))
       (let-values (((result calls)
                     (store! directory (write-key-file directory)
                             #:now (- %expiry 3600))))
         (eq? 'minted result)))))

  (test-assert "the previous token is revoked when asked"
    (call-with-work-dir
     (lambda (directory)
       (let ((key-file (write-key-file directory)))
         (store! directory key-file #:now (- %expiry 3600))
         (let-values (((result calls)
                       (store! directory key-file #:now %expiry
                               #:token "ghs_other" #:revoke-previous? #t)))
           (match (last calls)
             ((method path headers _)
              (and (eq? 'DELETE method)
                   (equal? '(bearer ghs_new)
                           (assq-ref headers 'authorization))))))))))

  (test-assert "the log never shows the token"
    (call-with-work-dir
     (lambda (directory)
       (let ((output
              (call-with-output-string
                (lambda (port)
                  (parameterize ((current-error-port port))
                    (store! directory (write-key-file directory)
                            #:now (- %expiry 3600)))))))
         (and (string-contains output "expires")
              (not (string-contains output "ghs_new"))))))))

(test-end "github-app")
