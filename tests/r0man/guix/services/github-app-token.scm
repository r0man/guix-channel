;;; Unit tests for (r0man guix services github-app-token): validation of
;;; the narrowing and the identity fields, the `github-app-token' account,
;;; the root-only activation and its permissions, the boot-time one-shot run
;;; as the minter account, and the refresh timer, which runs its command as
;;; that account and does not require the one-shot.

(define-module (test-r0man-guix-services-github-app-token)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (guix gexp)
  #:use-module (r0man guix github-app)
  #:use-module (r0man guix services github-app-token)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

(define %config
  (github-app-token-configuration
   (name "gascity")
   (client-id "Iv23liTEST")
   (installation-id 12345678)
   (private-key-file "/run/secrets/gascity-app.pem")
   (reader-group "gascity")
   (repositories '("guix-channel"))
   (permissions '((contents . "write") (pull_requests . "write")
                  (checks . "read") (statuses . "read") (actions . "read")))
   (shepherd-requirement '(sops-secret-gascity-app-pem))))

(define (rejected? thunk)
  (guard (c (#t #t))
    (thunk)
    #f))

(define (source gexp)
  "Return the approximate source of GEXP as a string, for inspection."
  (format #f "~s" (gexp->approximate-sexp gexp)))

(test-begin "github-app-token")


;;;
;;; Validation.
;;;

(test-group "validation"
  (test-assert "the example record is valid"
    (github-app-token-validate (list %config)))

  (test-assert "a repository instead of an installation id is valid"
    (github-app-token-validate
     (list (github-app-token-configuration
            (inherit %config)
            (installation-id #f)
            (repository "r0man/guix-channel")))))

  (test-assert "both installation-id and repository are rejected"
    (rejected?
     (lambda ()
       (github-app-token-validate
        (list (github-app-token-configuration
               (inherit %config) (repository "r0man/guix-channel")))))))

  (test-assert "neither installation-id nor repository is rejected"
    (rejected?
     (lambda ()
       (github-app-token-validate
        (list (github-app-token-configuration
               (inherit %config) (installation-id #f)))))))

  (test-assert "empty repositories are rejected"
    (rejected?
     (lambda ()
       (github-app-token-validate
        (list (github-app-token-configuration
               (inherit %config) (repositories '())))))))

  (test-assert "empty permissions are rejected"
    (rejected?
     (lambda ()
       (github-app-token-validate
        (list (github-app-token-configuration
               (inherit %config) (permissions '())))))))

  (for-each
   (lambda (permission)
     (test-assert (format #f "~a is rejected" permission)
       (rejected?
        (lambda ()
          (github-app-token-validate
           (list (github-app-token-configuration
                  (inherit %config)
                  (permissions (list '(contents . "write") permission)))))))))
   '((workflows . "write") (administration . "read") (secrets . "read")
     (actions . "write") (environments . "read") (pages . "write")))

  (test-assert "a root reader group is rejected"
    (rejected?
     (lambda ()
       (github-app-token-validate
        (list (github-app-token-configuration
               (inherit %config) (reader-group "root")))))))

  (test-assert "a name with a slash is rejected"
    (rejected?
     (lambda ()
       (github-app-token-validate
        (list (github-app-token-configuration
               (inherit %config) (name "../etc")))))))

  (test-assert "duplicate names are rejected"
    (rejected?
     (lambda ()
       (github-app-token-validate (list %config %config)))))

  (test-equal "the forbidden list matches the minter library's"
    %github-app-forbidden-permissions
    %github-app-token-forbidden-permissions))


;;;
;;; Accounts and activation.
;;;

(test-group "accounts"
  (define accounts (github-app-token-accounts (list %config)))
  (define account (find user-account? accounts))

  (test-equal "one system account and group"
    '("github-app-token" "github-app-token")
    (list (user-account-name account)
          (user-group-name (find user-group? accounts))))

  (test-assert "the account cannot log in and has no home"
    (and (user-account-system? account)
         (string=? "/var/empty" (user-account-home-directory account))
         (equal? '("/sbin/nologin")
                 (file-append-suffix (user-account-shell account)))))

  (test-assert "the account is in no reader group"
    (null? (user-account-supplementary-groups account)))

  (test-equal "no records, no account"
    '()
    (github-app-token-accounts '())))

(test-group "activation"
  (define activation (source (github-app-token-activation (list %config))))

  (test-equal "the default token directory"
    "/run/github-app-token/gascity"
    (github-app-token-directory %config))

  (test-equal "the token file consumers read"
    "/run/github-app-token/gascity/token"
    (github-app-token-file %config))

  (test-assert "the token root is root-owned 0755"
    (string-contains activation
                     "(ensure-directory \"/run/github-app-token\" 0 0 493)"))

  (test-assert "token directories are setgid 2750 for the reader group"
    (and (string-contains activation "1512")
         (string-contains activation
                          "(\"/run/github-app-token/gascity\" . \"gascity\")")))

  (test-assert "a missing reader group does not abort activation"
    (string-contains activation "(false-if-exception (group:gid (getgrnam group)))"))

  (test-assert "symlinks are removed, not followed"
    (string-contains activation "lstat"))

  (test-assert "the log file is root-owned 0640"
    (and (string-contains activation "/var/log/github-app-token.log")
         (string-contains activation "416"))))


;;;
;;; Shepherd services.
;;;

(test-group "shepherd services"
  (define services (github-app-token-shepherd-services (list %config)))
  (define one-shot (find shepherd-service-one-shot? services))
  (define timer (find (negate shepherd-service-one-shot?) services))

  (test-equal "a one-shot and a refresh timer per record"
    '((github-app-token-gascity) (github-app-token-gascity-refresh))
    (map shepherd-service-provision services))

  (test-equal "the consumer-facing service name"
    'github-app-token-gascity
    (github-app-token-shepherd-service-name %config))

  (test-equal "the one-shot requires networking and the key's provider"
    '(user-processes networking sops-secret-gascity-app-pem)
    (shepherd-service-requirement one-shot))

  (test-assert "the one-shot runs as github-app-token, in boot mode"
    (let ((start (source (shepherd-service-start one-shot))))
      (and (string-contains start "spawn-command")
           (string-contains start "#:user \"github-app-token\"")
           (string-contains start "\"--boot\""))))

  (test-equal "the timer does not require the one-shot"
    '(user-processes networking sops-secret-gascity-app-pem)
    (shepherd-service-requirement timer))

  (test-assert "the timer runs its command as github-app-token"
    (let ((start (source (shepherd-service-start timer))))
      (and (string-contains start "make-timer-constructor")
           (string-contains start "(cron-string->calendar-event \"*/15 * * * *\")")
           (string-contains start "#:user \"github-app-token\"")
           (string-contains start "#:log-file \"/var/log/github-app-token.log\"")
           (not (string-contains start "--boot")))))

  (test-assert "the timer can be triggered by hand"
    (equal? '(trigger)
            ;; The accessor is not exported.
            (map shepherd-action-name
                 ((@@ (gnu services shepherd) shepherd-service-actions)
                  timer))))

  (test-equal "the timer loads the timer module"
    '((shepherd service timer))
    (shepherd-service-modules timer))

  (test-assert "the program narrows and imports the minter library"
    (let ((program (source (program-file-gexp
                            (github-app-token-program %config)))))
      (and (string-contains program "mint-and-store-installation-token")
           (string-contains program "(quote (\"guix-channel\"))")
           (string-contains program "(contents . \"write\")")
           (string-contains program "/run/github-app-token/gascity"))))

  (test-assert "an invalid record fails when the services are built"
    (rejected?
     (lambda ()
       (github-app-token-shepherd-services
        (list (github-app-token-configuration
               (inherit %config) (repositories '()))))))))

(test-end "github-app-token")
