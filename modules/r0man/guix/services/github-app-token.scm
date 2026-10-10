;;; github-app-token.scm --- Mint GitHub App installation tokens periodically
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
;;; A system service that keeps a short-lived GitHub App installation token
;;; on disk for a consumer, typically one Gas City supervisor running as its
;;; own Unix user.  The consumer never sees the App's private key: a
;;; dedicated `github-app-token' system account reads the key, mints a token
;;; narrowed to explicit repositories and permissions (see
;;; (r0man guix github-app)), and writes it into a per-record directory that
;;; only the consumer's group can read:
;;;
;;;   /run/github-app-token/<name>/token             the token, 0640
;;;   /run/github-app-token/<name>/token.expires_at  its ISO 8601 expiry
;;;
;;; Each record yields two Shepherd services:
;;;
;;;   - the one-shot `github-app-token-<name>', which mints at boot, retrying
;;;     for about two minutes (a laptop's `networking' is up long before it
;;;     is online), and always succeeds so that a failed boot-time mint never
;;;     keeps its dependents down: the timer heals it;
;;;   - the timer `github-app-token-<name>-refresh', which re-mints whenever
;;;     the stored token expires within 30 minutes.  It does not require the
;;;     one-shot, so a failed one-shot never stops the refresh.
;;;
;;; Consumers list `github-app-token-<name>' in their own Shepherd
;;; requirements (the `shepherd-requirement' field of the Gas City service).
;;;
;;; Code:

(define-module (r0man guix services github-app-token)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages tls)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-34)
  #:export (%github-app-token-user
            %github-app-token-root
            %github-app-token-log-file
            %github-app-token-forbidden-permissions

            github-app-token-configuration
            github-app-token-configuration?
            github-app-token-configuration-name
            github-app-token-configuration-client-id
            github-app-token-configuration-installation-id
            github-app-token-configuration-repository
            github-app-token-configuration-private-key-file
            github-app-token-configuration-token-directory
            github-app-token-configuration-reader-group
            github-app-token-configuration-repositories
            github-app-token-configuration-permissions
            github-app-token-configuration-schedule
            github-app-token-configuration-revoke-previous?
            github-app-token-configuration-shepherd-requirement

            github-app-token-directory
            github-app-token-file
            github-app-token-shepherd-service-name
            github-app-token-validate
            github-app-token-program
            github-app-token-accounts
            github-app-token-activation
            github-app-token-shepherd-services
            github-app-token-service-type))


;;;
;;; Defaults.
;;;

(define %github-app-token-user
  ;; The account that reads the App private keys and writes the tokens.
  "github-app-token")

(define %github-app-token-root
  ;; Root-owned 0755; each record's token directory lives below it.
  "/run/github-app-token")

(define %github-app-token-log-file
  "/var/log/github-app-token.log")

(define %github-app-token-forbidden-permissions
  ;; A copy of `%github-app-forbidden-permissions' of (r0man guix
  ;; github-app), which loads gnutls and is therefore only imported into
  ;; store programs.  This copy rejects a bad record when the system is
  ;; built; the program checks again before every mint.
  '((workflows . #t)
    (administration . #t)
    (secrets . #t)
    (environments . #t)
    (pages . #t)
    (actions . ("write"))))


;;;
;;; Configuration.
;;;

(define-record-type* <github-app-token-configuration>
  github-app-token-configuration
  make-github-app-token-configuration
  github-app-token-configuration?
  (name              github-app-token-configuration-name)  ;string
  (client-id         github-app-token-configuration-client-id) ;string
  (installation-id   github-app-token-configuration-installation-id
                     (default #f))                 ;integer or #f
  (repository        github-app-token-configuration-repository
                     (default #f))                 ;"OWNER/NAME" or #f
  (private-key-file  github-app-token-configuration-private-key-file) ;string
  (token-directory   github-app-token-configuration-token-directory
                     (default #f))                 ;string or #f
  (reader-group      github-app-token-configuration-reader-group) ;string
  (repositories      github-app-token-configuration-repositories) ;strings
  (permissions       github-app-token-configuration-permissions) ;alist
  (schedule          github-app-token-configuration-schedule
                     (default "*/15 * * * *"))     ;cron string or gexp
  (revoke-previous?  github-app-token-configuration-revoke-previous?
                     (default #f))                 ;boolean
  (shepherd-requirement
   github-app-token-configuration-shepherd-requirement
   (default '())))                                 ;list of symbols

(define (github-app-token-directory config)
  "Return the directory holding the token of CONFIG: its `token-directory'
field, `/run/github-app-token/<name>' otherwise."
  (or (github-app-token-configuration-token-directory config)
      (string-append %github-app-token-root "/"
                     (github-app-token-configuration-name config))))

(define (github-app-token-file config)
  "Return the token file of CONFIG, the file consumers read."
  (string-append (github-app-token-directory config) "/token"))

(define (github-app-token-shepherd-service-name config)
  "Return the name of the one-shot Shepherd service of CONFIG, the service
consumers require: `github-app-token-<name>'."
  (string->symbol
   (string-append "github-app-token-"
                  (github-app-token-configuration-name config))))

(define (github-app-token-refresh-service-name config)
  (symbol-append (github-app-token-shepherd-service-name config) '-refresh))


;;;
;;; Validation.
;;;

(define (valid-name? name)
  (and (string? name)
       (not (string-null? name))
       (not (member name '("." "..")))
       (string-every (lambda (c)
                       (or (char-alphabetic? c) (char-numeric? c)
                           (memv c '(#\- #\_ #\.))))
                     name)))

(define-syntax-rule (invalid message argument ...)
  (raise (formatted-message message argument ...)))

(define (permission-name permission)
  (if (symbol? permission) permission (string->symbol permission)))

(define (validate-permissions name permissions)
  (unless (and (pair? permissions)
               (every (match-lambda
                        (((or (? symbol?) (? string?)) . (? string?)) #t)
                        (_ #f))
                      permissions))
    (invalid (G_ "github-app-token: ~a: 'permissions' must be a non-empty alist of \
(PERMISSION . LEVEL) pairs; an empty one would grant every permission the \
App has")
             name))
  (for-each
   (match-lambda
     ((permission . level)
      (match (assq (permission-name permission)
                   %github-app-token-forbidden-permissions)
        (#f #t)
        ((_ . #t)
         (invalid (G_ "github-app-token: ~a: the '~a' permission is forbidden for agent \
tokens")
                  name permission))
        ((_ . levels)
         (when (member level levels)
           (invalid (G_ "github-app-token: ~a: '~a: ~a' is forbidden for agent tokens")
                    name permission level))))))
   permissions))

(define (validate-configuration config)
  (match-record config <github-app-token-configuration>
    (name client-id installation-id repository private-key-file
     token-directory reader-group repositories permissions schedule
     shepherd-requirement)
    (unless (valid-name? name)
      (invalid (G_ "github-app-token: invalid name ~s (expected letters, digits, '.', '_' \
and '-')")
               name))
    (unless (and (string? client-id) (not (string-null? client-id)))
      (invalid (G_ "github-app-token: ~a: 'client-id' must be a non-empty string") name))
    (unless (or (and installation-id (not repository)
                     (exact-integer? installation-id)
                     (positive? installation-id))
                (and repository (not installation-id)
                     (string? repository)
                     (= 2 (length (string-split repository #\/)))
                     (every (negate string-null?)
                            (string-split repository #\/))))
      (invalid (G_ "github-app-token: ~a: set exactly one of 'installation-id' (a positive \
integer) and 'repository' (\"OWNER/NAME\")")
               name))
    (unless (and (string? private-key-file)
                 (string-prefix? "/" private-key-file))
      (invalid (G_ "github-app-token: ~a: 'private-key-file' must be an absolute file name")
               name))
    (unless (or (not token-directory)
                (and (string? token-directory)
                     (string-prefix? "/" token-directory)))
      (invalid (G_ "github-app-token: ~a: 'token-directory' must be an absolute file name")
               name))
    (unless (and (string? reader-group) (not (string-null? reader-group))
                 (not (string=? reader-group "root")))
      (invalid (G_ "github-app-token: ~a: 'reader-group' must name the consumer's own group")
               name))
    (unless (and (pair? repositories)
                 (every (lambda (repository)
                          (and (string? repository)
                               (not (string-null? repository))
                               (not (string-index repository #\/))))
                        repositories))
      (invalid (G_ "github-app-token: ~a: 'repositories' must be a non-empty list of \
repository names (without the owner); an empty one would cover every \
repository the App is installed on")
               name))
    (validate-permissions name permissions)
    (unless (or (string? schedule) (gexp? schedule))
      (invalid (G_ "github-app-token: ~a: 'schedule' must be a cron string or a calendar-event \
gexp")
               name))
    (unless (and (list? shepherd-requirement)
                 (every symbol? shepherd-requirement))
      (invalid (G_ "github-app-token: ~a: 'shepherd-requirement' must be a list of symbols")
               name))
    config))

(define (github-app-token-validate value)
  "Return VALUE, a list of <github-app-token-configuration> records, after
validating every record and checking that names and token directories are
unique.  Raise a `formatted-message' otherwise."
  (unless (and (list? value) (every github-app-token-configuration? value))
    (invalid (G_ "github-app-token: the service value must be a list of \
<github-app-token-configuration> records, got ~s")
             value))
  (for-each validate-configuration value)
  (let loop ((seen '())
             (keys (append (map github-app-token-configuration-name value)
                           (map github-app-token-directory value))))
    (match keys
      (() #t)
      ((key . rest)
       (when (member key seen)
         (invalid (G_ "github-app-token: ~s is used by more than one record") key))
       (loop (cons key seen) rest))))
  value)


;;;
;;; Program.
;;;

(define (github-app-token-program config)
  "Return a Guile program, as a file-like object, that mints and stores the
installation token of CONFIG as the `github-app-token' user.  With
`--boot' it retries for about two minutes and always exits 0; otherwise it
tries once and exits 1 on failure."
  (match-record config <github-app-token-configuration>
    (name client-id installation-id repository private-key-file reader-group
     repositories permissions revoke-previous?)
    (program-file
     (string-append "github-app-token-" name)
     ;; guile-gnutls must be an extension and GUILE_EXTENSIONS_PATH set by
     ;; hand: Shepherd runs programs with a clean environment (see
     ;; `github-actions-runner-mint-program').
     (with-extensions (list guile-json-4 guile-gnutls)
       (with-imported-modules '((guix base64)
                                (r0man guix github-api)
                                (r0man guix github-app))
         #~(begin
             (use-modules (r0man guix github-app))

             (define (log fmt . args)
               (apply format (current-error-port)
                      (string-append "github-app-token[" #$name "]: " fmt "~%")
                      args))

             (setenv "GUILE_EXTENSIONS_PATH"
                     #$(file-append guile-gnutls "/lib/guile/3.0/extensions"))
             (unless (getenv "SSL_CERT_DIR")
               (setenv "SSL_CERT_DIR" "/etc/ssl/certs"))

             (define token-directory #$(github-app-token-directory config))

             ;; /run is a tmpfs: activation recreates the directory at every
             ;; boot, but try anyway in case this runs before it.
             (unless (file-exists? token-directory)
               (false-if-exception (mkdir token-directory #o750)))

             (define (attempt)
               ;; Report the failure kind only: the irritants of the
               ;; library's own errors are status codes, never bodies, and
               ;; nothing here ever holds the JWT or the token.
               (catch #t
                 (lambda ()
                   (mint-and-store-installation-token
                    #:private-key-file #$private-key-file
                    #:client-id #$client-id
                    #:installation-id #$installation-id
                    #:repository #$repository
                    #:token-directory token-directory
                    #:reader-group #$reader-group
                    #:repositories '#$repositories
                    #:permissions '#$permissions
                    #:revoke-previous? #$revoke-previous?)
                   #t)
                 (lambda (key . args)
                   (log "minting failed: ~a"
                        (if (and (memq key '(misc-error system-error))
                                 (= 4 (length args)))
                            (apply format #f (cadr args) (caddr args))
                            key))
                   #f)))

             (if (member "--boot" (command-line))
                 ;; Back off for about two minutes, then give up quietly: the
                 ;; refresh timer heals a failed boot-time mint.
                 (let loop ((delays '(5 10 15 20 30 40)))
                   (cond ((attempt) (exit 0))
                         ((null? delays)
                          (log "giving up at boot; the refresh timer retries")
                          (exit 0))
                         (else
                          (sleep (car delays))
                          (loop (cdr delays)))))
                 (exit (if (attempt) 0 1)))))))))


;;;
;;; Accounts, activation and Shepherd services.
;;;

(define (github-app-token-accounts value)
  "Return the `github-app-token' account and group, when VALUE has records."
  (if (null? value)
      '()
      (list (user-group (name %github-app-token-user) (system? #t))
            (user-account
             (name %github-app-token-user)
             (group %github-app-token-user)
             (system? #t)
             (comment "GitHub App token minter")
             (home-directory "/var/empty")
             (create-home-directory? #f)
             (shell (file-append shadow "/sbin/nologin"))))))

(define (github-app-token-activation value)
  "Return the activation gexp of VALUE.  It runs as root and only touches
root-owned paths: the root-owned 0755 token root, each record's token
directory (setgid 2750, owned by `github-app-token' and the record's reader
group, so the tokens written into it inherit that group), and the log file.
A symlink planted at any of them is removed, never followed."
  (if (null? value)
      #~#t
      #~(begin
          (define (log fmt . args)
            (apply format (current-error-port)
                   (string-append "github-app-token: " fmt "~%") args))

          (define (remove-symlink file)
            (let ((st (false-if-exception (lstat file))))
              (when (and st (eq? 'symlink (stat:type st)))
                (log "removing the symlink ~a" file)
                (delete-file file))))

          (define (ensure-directory directory uid gid mode)
            (remove-symlink directory)
            (unless (file-exists? directory)
              (mkdir directory))
            (chown directory uid gid)
            (chmod directory mode))

          (define (mkdir-parents directory)
            (let ((parent (dirname directory)))
              (unless (or (string=? parent directory) (file-exists? parent))
                (mkdir-parents parent)
                (mkdir parent #o755))))

          (let* ((account (getpwnam #$%github-app-token-user))
                 (uid (passwd:uid account))
                 (fallback-gid (passwd:gid account)))
            (when (file-exists? #$(dirname %github-app-token-log-file))
              (remove-symlink #$%github-app-token-log-file)
              (unless (file-exists? #$%github-app-token-log-file)
                (close-port (open-file #$%github-app-token-log-file "a")))
              (chown #$%github-app-token-log-file 0 0)
              (chmod #$%github-app-token-log-file #o640))
            (mkdir-parents #$%github-app-token-root)
            (ensure-directory #$%github-app-token-root 0 0 #o755)
            (for-each
             (lambda (entry)
               (let* ((directory (car entry))
                      (group (cdr entry))
                      ;; A missing reader group must not abort the whole
                      ;; system activation: fall back to the minter's own
                      ;; group, which leaves the token unreadable.
                      (gid (or (false-if-exception (group:gid (getgrnam group)))
                               (begin
                                 (log "reader group ~a does not exist; ~a \
stays private"
                                      group directory)
                                 fallback-gid))))
                 (mkdir-parents directory)
                 (ensure-directory directory uid gid #o2750)))
             '#$(map (lambda (config)
                       (cons (github-app-token-directory config)
                             (github-app-token-configuration-reader-group
                              config)))
                     value))))))

(define (github-app-token-one-shot config)
  "Return the boot-time one-shot Shepherd service of CONFIG."
  (shepherd-service
   (documentation "Mint a GitHub App installation token at boot.")
   (provision (list (github-app-token-shepherd-service-name config)))
   (requirement `(user-processes networking
                  ,@(github-app-token-configuration-shepherd-requirement
                     config)))
   (one-shot? #t)
   ;; The program always exits 0 with `--boot', after backing off for about
   ;; two minutes, so its dependents start even when GitHub is unreachable.
   (start #~(lambda _
              (spawn-command (list #$(github-app-token-program config)
                                   "--boot")
                             #:user #$%github-app-token-user
                             #:group #$%github-app-token-user)
              #t))
   (stop #~(const #f))))

(define (github-app-token-refresh-timer config)
  "Return the refresh timer Shepherd service of CONFIG.  It is built by hand
rather than with `shepherd-timer', which cannot run its command as another
user, and it does not require the one-shot."
  (let ((schedule (github-app-token-configuration-schedule config)))
    (shepherd-service
     (documentation "Refresh a GitHub App installation token before it
expires.")
     (provision (list (github-app-token-refresh-service-name config)))
     (requirement `(user-processes networking
                    ,@(github-app-token-configuration-shepherd-requirement
                       config)))
     (modules '((shepherd service timer)))
     (start #~(make-timer-constructor
               #$(if (string? schedule)
                     #~(cron-string->calendar-event #$schedule)
                     schedule)
               (command (list #$(github-app-token-program config))
                        #:user #$%github-app-token-user
                        #:group #$%github-app-token-user)
               #:log-file #$%github-app-token-log-file
               #:wait-for-termination? #t))
     (stop #~(make-timer-destructor))
     (actions (list shepherd-trigger-action)))))

(define (github-app-token-shepherd-services value)
  "Return the Shepherd services of VALUE: for each record, the boot-time
one-shot and the refresh timer."
  (append-map (lambda (config)
                (list (github-app-token-one-shot config)
                      (github-app-token-refresh-timer config)))
              (github-app-token-validate value)))

(define github-app-token-service-type
  (service-type
   (name 'github-app-token)
   (extensions
    (list (service-extension account-service-type
                             github-app-token-accounts)
          (service-extension activation-service-type
                             (compose github-app-token-activation
                                      github-app-token-validate))
          (service-extension shepherd-root-service-type
                             github-app-token-shepherd-services)))
   (compose concatenate)
   (extend append)
   (default-value '())
   (description
    "Keep short-lived GitHub App installation tokens on disk.  The value is
a list of @code{github-app-token-configuration} records.  For each, a
dedicated @code{github-app-token} account reads the App's private key, mints
an installation token narrowed to explicit repositories and permissions,
and writes it to @file{/run/github-app-token/@var{name}/token}, readable
only by the record's reader group.  A boot-time one-shot,
@code{github-app-token-@var{name}}, mints the first token, and the timer
@code{github-app-token-@var{name}-refresh} re-mints before it expires.")))

;;; github-app-token.scm ends here
