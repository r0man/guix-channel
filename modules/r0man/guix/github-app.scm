;;; github-app.scm --- Mint GitHub App installation tokens
;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix channel.
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Mint short-lived GitHub App installation tokens in pure Guile.
;;;
;;; A GitHub App authenticates as itself with a JSON Web Token signed (RS256)
;;; with the App's private key, and exchanges it for an installation access
;;; token that lives one hour.  This module builds and signs the JWT with
;;; guile-gnutls, looks up an installation, mints a token narrowed to an
;;; explicit set of repositories and permissions, and stores it atomically
;;; next to a sidecar file holding its expiry, so that a periodic refresh
;;; re-mints only when the stored token is close to expiring.
;;;
;;; Files written by `mint-and-store-installation-token' in TOKEN-DIRECTORY:
;;;
;;;   token             the installation token, no trailing newline;
;;;   token.expires_at  its expiry as GitHub reports it, an ISO 8601 UTC
;;;                     timestamp such as "2026-10-08T12:00:00Z", followed by
;;;                     a newline.
;;;
;;; Both are mode 0640 and group-owned by the reader group, so only that
;;; group's users (one Gas City instance) can read them.
;;;
;;; The module loads (gnutls), so it is only ever imported into store
;;; programs (`with-imported-modules' plus `with-extensions' guile-gnutls and
;;; guile-json), never with `#:use-module' from a service module: that would
;;; load gnutls whenever the channel is compiled.  Neither the JWT, the
;;; token nor a raw error body is ever logged; the log shows the expiry and a
;;; short SHA-256 prefix of the token instead.
;;;
;;; Code:

(define-module (r0man guix github-app)
  #:use-module ((gnutls) #:hide (base64-encode base64-decode))
  #:use-module ((guix base64) #:select (base64-encode base64url-alphabet))
  #:use-module ((ice-9 binary-ports) #:select (get-bytevector-all))
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (r0man guix github-api)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (web response)
  #:use-module (web uri)
  #:re-export (%github-api-base
               %http-request)
  #:export (%github-app-forbidden-permissions
            github-app-validate-narrowing
            base64url-encode
            read-github-app-private-key
            github-app-jwt
            github-app-installation-id
            mint-installation-token
            revoke-installation-token
            parse-github-timestamp
            read-token-expiry
            token-fingerprint
            write-file-atomically
            mint-and-store-installation-token))


;;;
;;; Narrowing.
;;;

(define %github-app-forbidden-permissions
  ;; Installation-token permissions an agent must never hold, as
  ;; (PERMISSION . LEVELS) pairs; LEVELS is #t for every level.  `workflows'
  ;; would let an agent rewrite CI, `administration' change branch
  ;; protection, `secrets'/`environments' read or redirect deployment
  ;; credentials, `pages' publish under the owner's name, and `actions: write'
  ;; re-run or cancel workflows.  (r0man guix services github-app-token)
  ;; keeps a copy of this list for configuration-time validation; the tests
  ;; check that both agree.
  '((workflows . #t)
    (administration . #t)
    (secrets . #t)
    (environments . #t)
    (pages . #t)
    (actions . ("write"))))

(define (permission-name permission)
  (if (symbol? permission) permission (string->symbol permission)))

(define (github-app-validate-narrowing repositories permissions)
  "Raise an error unless REPOSITORIES is a non-empty list of repository
names and PERMISSIONS a non-empty alist of (PERMISSION . LEVEL) pairs, none
of which is in `%github-app-forbidden-permissions'.  Return #t otherwise.

An installation token minted without `repositories' covers every repository
the App is installed on, and one minted without `permissions' carries every
permission the App was granted, so both are mandatory here."
  (unless (and (pair? repositories) (every string? repositories)
               (every (negate string-null?) repositories))
    (error "github-app: a non-empty list of repository names is required"))
  (unless (and (pair? permissions)
               (every (match-lambda
                        (((or (? symbol?) (? string?)) . (? string?)) #t)
                        (_ #f))
                      permissions))
    (error "github-app: an explicit, non-empty permissions alist is required"))
  (for-each
   (match-lambda
     ((permission . level)
      (match (assq (permission-name permission)
                   %github-app-forbidden-permissions)
        (#f #t)
        ((_ . #t)
         (error "github-app: forbidden permission" permission))
        ((_ . levels)
         (when (member level levels)
           (error "github-app: forbidden permission level"
                  permission level))))))
   permissions)
  #t)


;;;
;;; JSON Web Token.
;;;

(define (base64url-encode bv)
  "Return the unpadded base64url encoding of the bytevector BV."
  (base64-encode bv 0 (bytevector-length bv) #f #t base64url-alphabet))

(define (read-github-app-private-key file)
  "Return the GitHub App private key stored in FILE, a PEM file as GitHub
generates it (PKCS#1 \"RSA PRIVATE KEY\"), as a gnutls private key."
  (let ((pem (call-with-input-file file get-bytevector-all #:binary #t)))
    (x509-private-key->private-key
     (import-x509-private-key pem x509-certificate-format/pem)
     '())))

(define* (github-app-jwt private-key client-id #:key (now (current-time)))
  "Return a JSON Web Token, as a string, that authenticates the GitHub App
CLIENT-ID (its client ID, or its numeric App ID as a string) and is signed
(RS256) with PRIVATE-KEY, a gnutls private key.

The token is backdated 60 seconds to absorb clock drift and expires 8
minutes after NOW, under GitHub's 10-minute maximum."
  (let* ((header (scm->json-string '((alg . "RS256") (typ . "JWT"))))
         (payload (scm->json-string
                   `((iat . ,(- now 60))
                     (exp . ,(+ now 480))
                     (iss . ,client-id))))
         (signing-input (string-append
                         (base64url-encode (string->utf8 header))
                         "."
                         (base64url-encode (string->utf8 payload))))
         (signature (private-key-sign-data private-key
                                           sign-algorithm/rsa-sha256
                                           (string->utf8 signing-input)
                                           '())))
    (string-append signing-input "." (base64url-encode signature))))


;;;
;;; REST API.
;;;

(define (api-headers bearer)
  "Return the headers of a GitHub REST API request authenticated with the
bearer token BEARER (a JWT or an installation token).  The bearer token is
a symbol because Guile's header validator only accepts symbols there."
  `((accept . ((application/vnd.github+json)))
    (authorization . (bearer ,(string->symbol bearer)))
    (x-github-api-version . "2022-11-28")
    (user-agent . "r0man-guix-github-app-token")))

(define (body->string body)
  (cond ((bytevector? body) (utf8->string body))
        ((string? body) body)
        (else "")))

(define (api-request method path bearer body)
  "Perform the GitHub API request METHOD on PATH (relative to
`%github-api-base') authenticated with BEARER, sending BODY.  Return two
values: the response code and the response body as a string."
  (let-values (((response response-body)
                ((%http-request) (string->uri
                                  (string-append (%github-api-base) path))
                                 #:method method
                                 #:headers (api-headers bearer)
                                 #:body body)))
    (values (response-code response) (body->string response-body))))

(define (github-app-installation-id jwt repository)
  "Return the numeric ID of the installation of the GitHub App
authenticated by JWT on REPOSITORY, an \"OWNER/NAME\" string.  Raise an
error, carrying only the status code, when the lookup fails."
  (let-values (((code body)
                (api-request 'GET
                             (string-append "/repos/" repository
                                            "/installation")
                             jwt "")))
    (if (eqv? code 200)
        (or (assoc-ref (json-string->scm body) "id")
            (error "github-app: installation response has no id" repository))
        (error "github-app: installation lookup failed" repository code))))

(define* (mint-installation-token jwt installation-id
                                  #:key repositories permissions)
  "Mint an installation token for INSTALLATION-ID with the GitHub App
authenticated by JWT, narrowed to REPOSITORIES (a non-empty list of
repository names, without the owner) and PERMISSIONS (an alist such as
'((contents . \"write\"))).  Return two values: the token and its expiry
timestamp string.  Raise an error, carrying only the status code, when
GitHub refuses."
  (github-app-validate-narrowing repositories permissions)
  (let-values (((code body)
                (api-request
                 'POST
                 (string-append "/app/installations/"
                                (if (number? installation-id)
                                    (number->string installation-id)
                                    installation-id)
                                "/access_tokens")
                 jwt
                 (scm->json-string
                  `((repositories . ,(list->vector repositories))
                    (permissions
                     . ,(map (match-lambda
                               ((permission . level)
                                (cons (permission-name permission) level)))
                             permissions)))))))
    (if (eqv? code 201)
        (let* ((json (json-string->scm body))
               (token (assoc-ref json "token"))
               (expires-at (assoc-ref json "expires_at")))
          (unless (and (string? token) (string? expires-at))
            (error "github-app: access-token response lacks token or expiry"))
          (values token expires-at))
        (error "github-app: minting the installation token failed"
               installation-id code))))

(define (revoke-installation-token token)
  "Revoke the installation token TOKEN.  Return #t when GitHub revoked it
and #f otherwise (an expired token cannot be revoked, which is fine)."
  (let-values (((code body)
                (api-request 'DELETE "/installation/token" token "")))
    (eqv? code 204)))


;;;
;;; Storage.
;;;

(define (parse-github-timestamp string)
  "Return the number of seconds since the epoch of STRING, an ISO 8601 UTC
timestamp as GitHub returns it (\"2026-10-08T12:00:00Z\"), or #f when STRING
does not parse."
  (false-if-exception
   (car (mktime (car (strptime "%Y-%m-%dT%H:%M:%SZ" (string-trim-both string)))
                "UTC"))))

(define (read-token-expiry token-directory)
  "Return the expiry, in seconds since the epoch, of the token stored in
TOKEN-DIRECTORY, or #f when there is no token or no parseable sidecar."
  (let ((token (string-append token-directory "/token"))
        (sidecar (string-append token-directory "/token.expires_at")))
    (and (file-exists? token)
         (file-exists? sidecar)
         (parse-github-timestamp
          (call-with-input-file sidecar get-string-all)))))

(define (token-fingerprint token)
  "Return a short, non-reversible fingerprint of TOKEN for the logs: the
first 12 hex digits of its SHA-256."
  (let ((digest (hash-direct digest/sha256 (string->utf8 token))))
    (string-take
     (string-concatenate
      (map (lambda (byte)
             (string-pad (number->string byte 16) 2 #\0))
           (bytevector->u8-list digest)))
     12)))

(define (group-id group)
  "Return the GID of GROUP, a name or a number, or #f when it is unknown."
  (cond ((integer? group) group)
        ((string? group)
         (false-if-exception (group:gid (getgrnam group))))
        (else #f)))

(define* (write-file-atomically file content #:key group (mode #o640))
  "Write the string CONTENT to FILE atomically: create FILE.tmp exclusively
under a 077 umask, set MODE and the group GROUP (when given and resolvable),
flush it to disk, and rename it over FILE.  A reader thus sees either the
previous complete file or the new one, never a partial or world-readable
one."
  (let ((temporary (string-append file ".tmp"))
        (gid (and group (group-id group)))
        (old-umask (umask #o077)))
    (dynamic-wind
      (const #t)
      (lambda ()
        ;; A leftover from an interrupted run; O_EXCL below then guarantees
        ;; the file is ours and freshly created.
        (when (false-if-exception (lstat temporary))
          (delete-file temporary))
        (let ((port (open temporary (logior O_WRONLY O_CREAT O_EXCL) #o600)))
          (dynamic-wind
            (const #t)
            (lambda ()
              (chmod port mode)
              (when (and gid (not (= gid (stat:gid (stat port)))))
                ;; Inside a setgid directory the group is already right;
                ;; otherwise this needs membership in GROUP.
                (chown port -1 gid))
              (put-string port content)
              (force-output port)
              (fsync port))
            (lambda ()
              (close-port port))))
        (rename-file temporary file))
      (lambda ()
        (umask old-umask)))))

(define (log-message fmt . args)
  (apply format (current-error-port)
         (string-append "github-app-token: " fmt "~%")
         args))

(define* (mint-and-store-installation-token
          #:key private-key-file client-id installation-id repository
          token-directory reader-group repositories permissions
          (refresh-margin 1800) (revoke-previous? #f) (now (current-time))
          force?)
  "Mint an installation token of the GitHub App CLIENT-ID, whose private key
is in PRIVATE-KEY-FILE, and store it in TOKEN-DIRECTORY (see the
commentary), readable by READER-GROUP.  The installation is INSTALLATION-ID
or, when that is #f, the one on REPOSITORY (\"OWNER/NAME\").  The token is
narrowed to REPOSITORIES and PERMISSIONS (see `mint-installation-token').

Unless FORCE? is true, nothing is minted while the stored token is valid for
more than REFRESH-MARGIN seconds after NOW.  When REVOKE-PREVIOUS? is true,
the replaced token is revoked after the new one is stored.  Return 'minted or
'skipped."
  (github-app-validate-narrowing repositories permissions)
  (let ((expiry (read-token-expiry token-directory))
        (token-file (string-append token-directory "/token"))
        (sidecar (string-append token-directory "/token.expires_at")))
    (if (and (not force?) expiry (> (- expiry now) refresh-margin))
        (begin
          (log-message "token for ~a still valid for ~a s; skipping"
               client-id (- expiry now))
          'skipped)
        (let* ((key (read-github-app-private-key private-key-file))
               (jwt (github-app-jwt key client-id #:now now))
               (installation (or installation-id
                                 (github-app-installation-id jwt repository)))
               (previous (and revoke-previous?
                              (file-exists? token-file)
                              (string-trim-both
                               (call-with-input-file token-file
                                 get-string-all)))))
          (let-values (((token expires-at)
                        (mint-installation-token
                         jwt installation
                         #:repositories repositories
                         #:permissions permissions)))
            (write-file-atomically token-file token #:group reader-group)
            (write-file-atomically sidecar (string-append expires-at "\n")
                                   #:group reader-group)
            (log-message "minted token sha256:~a for installation ~a, expires ~a"
                 (token-fingerprint token) installation expires-at)
            (when (and previous (not (string-null? previous))
                       (not (string=? previous token)))
              (log-message "revoked previous token sha256:~a: ~a"
                   (token-fingerprint previous)
                   (if (false-if-exception (revoke-installation-token previous))
                       "yes" "no")))
            'minted)))))

;;; github-app.scm ends here
