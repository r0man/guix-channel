;;; Mint and revoke GitHub Actions runner registration tokens.
;;;
;;; This module implements the GitHub REST API calls needed for the
;;; lifecycle of a VM-backed runner: minting a registration token with a
;;; personal access token (PAT), and removing a registered runner.
;;;
;;; The module deliberately depends on nothing beyond Guile core and
;;; guile-json, so that it can be imported both from the service module
;;; and from store programs built with 'program-file' (see the
;;; 'github-actions-vm-mint-program' and
;;; 'github-actions-vm-remove-runner-program' procedures of
;;; (r0man guix services github-actions-vm)).

(define-module (r0man guix services github-actions-vm-mint)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (%github-api-base
            %http-request
            github-api-url
            read-pat-file
            mint-registration-token
            mint-and-store-registration-token
            remove-github-actions-runner))

(define %http-request
  ;; The HTTP request procedure, a parameter so that tests can stub it
  ;; out; defaults to Guile core's 'http-request' (see the design doc
  ;; for why (guix http-client) could not be used: it is GET-only).
  (make-parameter http-request))

(define %github-api-base
  ;; Base URL of the GitHub REST API.  Overridable with the
  ;; GITHUB_API_BASE environment variable, which lets tests point the
  ;; store programs at a mock API server.
  (make-parameter (or (getenv "GITHUB_API_BASE") "https://api.github.com")))

(define (github-api-url url path)
  "Return the full GitHub API URL for the API PATH, derived from URL,
the web URL of a repository, organization, or user (for instance
https://github.com/r0man/burningswell or https://github.com/my-org).

Repository and organization runners use different API paths
(/repos/{owner}/{repo}/... vs /orgs/{org}/...), so the number of path
segments of URL decides which one to use.  For hosts other than
github.com (GitHub Enterprise Server), the API is served from the same
host under /api/v3, and %github-api-base is ignored."
  (let* ((uri (string->uri url))
         (host (uri-host uri))
         (base (if (string=? host "github.com")
                   (%github-api-base)
                   (string-append (symbol->string (uri-scheme uri))
                                  "://" host "/api/v3")))
         (segments (filter (lambda (segment)
                             (not (string=? segment "")))
                           (split-and-decode-uri-path (uri-path uri))))
         (segments (match segments
                     ;; Already an organization URL: keep as-is.
                     (("orgs" . _) segments)
                     ;; owner/repo
                     ((_ _) (cons "repos" segments))
                     ;; An organization (or user) URL.
                     ((org) (list "orgs" org))
                     (_ (error "github-actions-vm: cannot derive an API path from" url))))
         (base-path (string-join (cons "" segments) "/")))
    (string-append base base-path path)))

(define (read-pat-file pat-file)
  "Return the personal access token stored in PAT-FILE, with
surrounding whitespace stripped."
  (string-trim-both (call-with-input-file pat-file get-string-all)))

(define (api-headers pat)
  "Return the HTTP headers for an authenticated GitHub API request.
Note: the Accept and Authorization headers are given in the form
Guile's HTTP library expects (parsed structures); the Authorization
header writes as \"Bearer <token>\".  The token is turned into a
symbol because Guile's header validator only accepts key-value lists
of symbols, and this is the shape that writes as a plain bearer token."
  `((accept . ((application/vnd.github+json)))
    (authorization . (bearer ,(string->symbol pat)))
    (user-agent . "github-actions-runner-vm-guix")))

(define (mint-registration-token pat url)
  "Return a new runner registration token, minted with the personal
access token PAT at the repository, organization, or user URL.

Registration tokens remain valid for one hour and are reusable within
that hour.  Raise an error when GitHub responds with a non-2xx status
or a body without a token field."
  (let-values (((response body)
                ((%http-request) (string->uri
                                  (github-api-url
                                   url "/actions/runners/registration-token"))
                                 #:method 'POST
                                 #:headers (api-headers pat)
                                 #:body "")))
    (let ((code (response-code response)))
      (if (memv code '(200 201))
          (or (assoc-ref (json-string->scm body) "token")
              (error "github-actions-vm: registration-token response has no token field"
                     code body))
          (error "github-actions-vm: registration-token request failed"
                 code body)))))

(define* (mint-and-store-registration-token pat url token-file
                                           #:key (reuse-within 3300))
  "Mint a registration token and write it to TOKEN-FILE with mode 0600,
returning #t.

If TOKEN-FILE already exists and was written less than REUSE-WITHIN
seconds ago, it is kept as-is: registration tokens are valid for one
hour and are reusable, so a still-fresh token is reused instead of
putting more pressure on the API rate limits."
  (if (and (file-exists? token-file)
           (< (- (current-time) (stat:mtime (stat token-file))) reuse-within))
      #t
      (begin
        (let ((token (mint-registration-token pat url)))
          (call-with-output-file token-file
            (lambda (port) (display token port)))
          (chmod token-file #o600)
          #t))))

(define (remove-github-actions-runner pat url name)
  "Remove the runner registered as NAME (best effort), using the
personal access token PAT at the repository, organization, or user URL.

The runner is looked up by name in the runners list API and then
deleted with the 'force' parameter, which also removes runners that are
currently executing a job.  Return #t when the runner was removed, and
#f when no runner by that name is registered.  Raise an error when an
API request fails."
  (let-values (((response body)
                ((%http-request) (string->uri
                                  (github-api-url
                                   url "/actions/runners?per_page=100"))
                                 #:method 'GET
                                 #:headers (api-headers pat)
                                 #:body "")))
    (let ((code (response-code response)))
      (if (eqv? code 200)
          (let* ((runners (or (assoc-ref (json-string->scm body) "runners")
                              '()))
                 ;; guile-json parses JSON arrays as vectors.
                 (runners (cond ((vector? runners) (vector->list runners))
                                ((list? runners) runners)
                                (else '())))
                 (runner (find (lambda (runner)
                                 (string=? (or (assoc-ref runner "name") "")
                                           name))
                               runners)))
            (if runner
                (let-values (((response body)
                              ((%http-request)
                               (string->uri
                                (string-append
                                 (github-api-url url "/actions/runners/")
                                 (number->string (assoc-ref runner "id"))
                                 "?force=true"))
                               #:method 'DELETE
                               #:headers (api-headers pat)
                               #:body "")))
                  (let ((code (response-code response)))
                    (if (memv code '(200 204))
                        #t
                        (error "github-actions-vm: runner removal failed"
                               name code body))))
                #f))
          (error "github-actions-vm: runner list request failed" code body)))))
