(define-module (r0man guix services github-actions)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages tls)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix packages github-actions)
  #:use-module (r0man guix services github-actions-vm-mint)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (github-actions-runner-configuration
            github-actions-runner-configuration?
            github-actions-runner-configuration-package
            github-actions-runner-configuration-user
            github-actions-runner-configuration-group
            github-actions-runner-configuration-work-dir
            github-actions-runner-configuration-url
            github-actions-runner-configuration-token
            github-actions-runner-configuration-name
            github-actions-runner-configuration-labels
            github-actions-runner-configuration-replace?
            github-actions-runner-configuration-extra-registration-args
            github-actions-runner-configuration-environment-variables
            github-actions-runner-configuration-supplementary-groups
            github-actions-runner-configuration-ephemeral?
            github-actions-runner-configuration-requirements
            github-actions-runner-configuration-shutdown-file
            github-actions-runner-configuration-registration-marker
            github-actions-runner-configuration-log-file
            github-actions-runner-start-script
            github-actions-runner-service-type
            github-actions-runner-mint-program
            github-actions-runner-token-configuration
            github-actions-runner-token-configuration?
            github-actions-runner-token-configuration-url
            github-actions-runner-token-configuration-pat-file
            github-actions-runner-token-configuration-token-file
            github-actions-runner-token-configuration-runner-config-file
            github-actions-runner-token-configuration-user
            github-actions-runner-token-configuration-group
            github-actions-runner-token-configuration-requirements
            github-actions-runner-token-mint-script
            github-actions-runner-token-mint-service-type))

(define-record-type* <github-actions-runner-configuration>
  github-actions-runner-configuration
  make-github-actions-runner-configuration
  github-actions-runner-configuration?
  (package  github-actions-runner-configuration-package
            (default github-actions-runner))
  (user     github-actions-runner-configuration-user
            (default "github-actions-runner"))
  (group    github-actions-runner-configuration-group
            (default "github-actions-runner"))
  (work-dir github-actions-runner-configuration-work-dir
            (default "/var/lib/github-actions-runner"))
  (url      github-actions-runner-configuration-url
            (default #f))                       ;string or #f
  (token    github-actions-runner-configuration-token
            (default #f))                       ;string, file-like, or #f
  (name     github-actions-runner-configuration-name
            (default #f))                       ;string or #f
  (labels   github-actions-runner-configuration-labels
            (default '()))                      ;list of strings
  (replace? github-actions-runner-configuration-replace?
            (default #f))
  (extra-registration-args
   github-actions-runner-configuration-extra-registration-args
   (default '()))                               ;list of strings
  (environment-variables
   github-actions-runner-configuration-environment-variables
   (default '()))                               ;list of "KEY=value" strings
  (supplementary-groups
   github-actions-runner-configuration-supplementary-groups
   (default '()))                               ;list of group name strings
  (ephemeral? github-actions-runner-configuration-ephemeral?
              (default #f))
  (requirements github-actions-runner-configuration-requirements
                (default '()))                  ;list of shepherd symbols
  (shutdown-file
   github-actions-runner-configuration-shutdown-file
   (default #f))                                ;string path or #f
  (registration-marker
   github-actions-runner-configuration-registration-marker
   (default #f))                                ;string path or #f
  (log-file
   github-actions-runner-configuration-log-file
   (default "/var/log/github-actions-runner.log")))

(define %runner-child-path-packages
  ;; Packages whose bin directories head the PATH the runner's launchers
  ;; inherit.  Shepherd starts services with a clean environment, and the
  ;; launchers of the actions-runner package, as well as GitHub's own
  ;; config.sh and run.sh behind them, call bare commands such as mkdir,
  ;; cp, chmod, grep, and sed.
  (list coreutils grep sed findutils))

(define* (github-actions-runner-start-script
          #:key
          (package github-actions-runner)
          (work-dir #f)
          (url #f)
          (token #f)
          (name #f)
          (labels '())
          (replace? #f)
          (extra-registration-args '())
          (environment-variables '())
          (ephemeral? #f)
          (shutdown-file #f)
          (registration-marker #f))
  "Return a Guile program, as a file-like object, that prepares the
writable runner work directory WORK-DIR (populated on first use by the
launchers of the actions-runner package), registers the runner with
GitHub unless it has been registered before, and finally execs the
runner.

The program depends on no ambient environment: it refers to every
external program by its store file name, and sets a PATH made of
coreutils, grep, sed, and findutils in front of the inherited one (if
any) before it runs the launchers of PACKAGE, which is what Shepherd's
clean service environment requires.  HOME and ACTIONS_RUNNER_DIR are
set to WORK-DIR.

WORK-DIR is a literal directory name, or #f for
@file{$XDG_DATA_HOME/actions-runner}, falling back to
@file{$HOME/.local/share/actions-runner}, resolved when the program
starts (the default of the actions-runner launchers).

TOKEN is either a registration token string, a file-like object holding
the token, an absolute file name holding the token (read when the
program starts, with carriage returns and newlines removed), or #f.

ENVIRONMENT-VARIABLES is a list of literal \"NAME=value\" strings, set
after HOME, ACTIONS_RUNNER_DIR, and PATH, so they may override any of
them.  No shell expansion takes place.

When EPHEMERAL? is true, the program runs the runner without exec'ing
it (its exit status matters) and, when SHUTDOWN-FILE is set, touches
that file once the runner has exited, so that an external supervisor
can tear down the machine the runner lived in.  On failure (missing
credentials, a failed registration, or a runner exiting with a non-zero
status), the program waits for 300 seconds before touching the shutdown
file, which backs off respawning a broken runner.  When
REGISTRATION-MARKER is set, the program touches it after a successful
registration, which lets an external supervisor expire credentials that
are no longer needed.  All three exist for the VM-backed runner
service; the default values keep the ordinary behavior: exec the runner
and never touch any marker file.

SUPPLEMENTARY-GROUPS makes the runner account join additional groups
such as @code{docker}, and REQUIREMENTS adds shepherd services to the
runner service's requirement list (both are consumed by the shepherd
service and the accounts extensions, not by the start program)."
  (define token-file
    (cond ((file-like? token) token)
          ((and (string? token) (string-prefix? "/" token)) token)
          (else #f)))

  (define token-string
    (and (string? token) (not token-file) token))

  (define registration-args
    (append (if name (list "--name" name) '())
            (if (null? labels) '() (list "--labels" (string-join labels ",")))
            (if replace? (list "--replace") '())
            extra-registration-args))

  (program-file
   "github-actions-runner-start"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 textual-ports))

         (define (log fmt . args)
           (apply format (current-error-port)
                  (string-append "github-actions-runner: " fmt "~%")
                  args))

         (define (touch file)
           (close-port (open-file file "a"))
           (utime file))

         (define runner-dir
           (or #$work-dir
               (string-append
                (or (getenv "XDG_DATA_HOME")
                    (string-append (or (getenv "HOME")
                                       (begin
                                         (log "neither XDG_DATA_HOME nor HOME is set")
                                         (exit 1)))
                                   "/.local/share"))
                "/actions-runner")))

         (define url #$url)
         (define shutdown-file #$shutdown-file)
         (define registration-marker #$registration-marker)
         (define config #$(file-append package "/bin/actions-runner-config"))
         (define run #$(file-append package "/bin/actions-runner"))

         (define (read-token)
           (let ((file #$token-file))
             (if file
                 (and (file-exists? file)
                      (list->string
                       (filter (lambda (ch)
                                 (not (memv ch '(#\return #\newline))))
                               (string->list
                                (call-with-input-file file get-string-all)))))
                 #$token-string)))

         (define (fail fmt . args)
           (apply log fmt args)
           (when shutdown-file
             (sleep 300)
             (false-if-exception (touch shutdown-file)))
           (exit 1))

         (setenv "ACTIONS_RUNNER_DIR" runner-dir)
         (setenv "HOME" runner-dir)
         (setenv "PATH"
                 (string-join
                  (append (list #$@(map (lambda (p) (file-append p "/bin"))
                                        %runner-child-path-packages))
                          (let ((path (getenv "PATH")))
                            (if path (list path) '())))
                  ":"))
         (for-each (lambda (var)
                     (let ((index (string-index var #\=)))
                       (setenv (substring var 0 index)
                               (substring var (+ index 1)))))
                   '#$environment-variables)

         (mkdir-p runner-dir)
         (chdir runner-dir)

         (unless (file-exists? (string-append runner-dir "/.runner"))
           (let ((token (read-token)))
             (when (or (not url) (string-null? url)
                       (not token) (string-null? token))
               (fail "~a is not registered with GitHub, and no URL or \
registration token was provided.  Either set the 'url' and 'token' fields \
of github-actions-runner-configuration, or register the runner manually \
by running: ~a"
                     runner-dir config))
             (log "registering runner in ~a" runner-dir)
             (unless (zero? (apply system* config
                                   "--unattended" "--url" url
                                   "--token" token
                                   '#$registration-args))
               (fail "registration failed"))
             (when registration-marker
               (false-if-exception
                (begin
                  (mkdir-p (dirname registration-marker))
                  (touch registration-marker))))))

         (if #$ephemeral?
             (let* ((status (system* run))
                    (code (or (status:exit-val status) 1)))
               (log "runner exited with status ~a" code)
               (when shutdown-file
                 (unless (zero? code)
                   (sleep 300))
                 (false-if-exception (touch shutdown-file)))
               (exit code))
             (execl run run))))))

(define (github-actions-runner-shepherd-service config)
  (match-record config <github-actions-runner-configuration>
    (package user group work-dir url token name labels replace?
             extra-registration-args environment-variables
             supplementary-groups ephemeral? requirements
             shutdown-file registration-marker log-file)
    (let ((script
           (github-actions-runner-start-script
            #:package package
            #:work-dir work-dir
            #:url url
            #:token token
            #:name name
            #:labels labels
            #:replace? replace?
            #:extra-registration-args extra-registration-args
            #:environment-variables environment-variables
            #:ephemeral? ephemeral?
            #:shutdown-file shutdown-file
            #:registration-marker registration-marker)))
      (list (shepherd-service
             (documentation "Run a self-hosted GitHub Actions runner.")
             (provision '(github-actions-runner))
             (requirement `(user-processes networking ,@requirements))
             (respawn? (not ephemeral?))
             ;; shepherd's exec-command clears the supplementary
             ;; groups of the process ('setgroups'), so the groups
             ;; configured for the runner account (e.g. "docker" for
             ;; access to /var/run/docker.sock) must be passed
             ;; explicitly; the account's groups alone are not enough.
             (start #~(make-forkexec-constructor
                       (list #$script)
                       #:user #$user
                       #:group #$group
                       ;; Note the quote: a plain list value must not
                       ;; be unquoted into the gexp, or its elements
                       ;; end up in evaluated position.
                       #:supplementary-groups '#$supplementary-groups
                       #:log-file #$log-file))
             (stop #~(make-kill-destructor)))))))

(define (github-actions-runner-accounts config)
  (match-record config <github-actions-runner-configuration>
    (user group work-dir supplementary-groups)
    (append
     ;; Groups the runner account joins (e.g. "docker" for access to
     ;; /var/run/docker.sock); create them as system groups.
     (map (lambda (name)
            (user-group (name name) (system? #t)))
          (delete group (delete-duplicates supplementary-groups)))
     ;; Nothing to create for the built-in root account; the VM-backed
     ;; service may configure the runner to run as root.
     (if (string=? user "root")
         '()
         (list (user-group
                (name group)
                (system? #t))
               (user-account
                (name user)
                (group group)
                (system? #t)
                (comment "GitHub Actions runner")
                (home-directory work-dir)
                (supplementary-groups
                 (delete group (delete-duplicates supplementary-groups)))
                (shell (file-append shadow "/sbin/nologin"))))))))

(define (github-actions-runner-activation config)
  (match-record config <github-actions-runner-configuration>
    (user group work-dir)
    #~(begin
        (use-modules (guix build utils))
        (let ((uid (if (string=? #$user "root") 0 (passwd:uid (getpwnam #$user))))
              (gid (if (string=? #$group "root") 0 (group:gid (getgrnam #$group)))))
          (for-each
           (lambda (dir)
             (mkdir-p dir)
             (chown dir uid gid)
             (chmod dir #o755))
           (list #$work-dir
                 (string-append #$work-dir "/_work")))))))

(define github-actions-runner-service-type
  (service-type
   (name 'github-actions-runner)
   (extensions
    (list (service-extension shepherd-root-service-type
                             github-actions-runner-shepherd-service)
          (service-extension activation-service-type
                             github-actions-runner-activation)
          (service-extension account-service-type
                             github-actions-runner-accounts)
          (service-extension profile-service-type
                             (lambda (config)
                               (list
                                (github-actions-runner-configuration-package
                                 config))))))
   (default-value (github-actions-runner-configuration))
   (description
    "Run a self-hosted GitHub Actions runner as a daemon.  The runner is
registered with GitHub on first start using the @code{url} and
@code{token} configuration fields, and executes jobs dispatched by
GitHub Actions workflows from @code{work-dir}.")))

;;; --- registration token minting --------------------------------------
;;;
;;; A GitHub runner registration token is valid for ONE HOUR only, so it
;;; cannot be stored declaratively: the persistent runner service wants
;;; one the moment it starts unregistered, and never again.  This
;;; service type is the general, native building block for that: a
;;; one-shot Shepherd service that, at every boot where the runner is
;;; not yet registered, mints a fresh registration token from a PAT
;;; (the same REST endpoint the VM-backed runner service uses, via the
;;; pure-Guile module it shares) and hands it to the runner's user.
;;;
;;; The PAT is consumed only by this one-shot, which the deploying OS
;;; is expected to run as a user that can read it (typically root, with
;;; the PAT decrypted from a secrets store) and that the runner's own
;;; account does NOT have to be able to read.  The runner receives only
;;; the minted token, which expires within the hour and grants nothing
;;; but registering runners.  Pair this service with
;;; `github-actions-runner-service-type' whose `token' field names the
;;; same TOKEN-FILE, and whose `requirements' include this service's
;;; provision.

(define (github-actions-runner-mint-program)
  "Return a store program that mints a runner registration token: it
takes a PAT-FILE, a repository/organization URL, and a TOKEN-FILE as
arguments, minting a fresh token when needed (reusing one younger than
55 minutes) and writing it to TOKEN-FILE with mode 0600.  GITHUB_API_BASE
overrides the API base URL for testing."
  ;; Note: guile-gnutls must be among the extensions and its foreign
  ;; library directory exported as GUILE_EXTENSIONS_PATH: shepherd runs
  ;; programs with a clean environment, where the ambient
  ;; GUILE_EXTENSIONS_PATH (which is how the (gnutls) module is
  ;; normally found) is absent, and (web client) HTTPS calls would fail
  ;; with gnutls-not-available.
  (program-file "github-actions-runner-mint-registration-token"
                (with-extensions (list guile-json-4 guile-gnutls)
                  (with-imported-modules '((r0man guix services
                                           github-actions-vm-mint))
                    #~(begin
                      (use-modules (ice-9 match)
                                   (ice-9 textual-ports)
                                   (r0man guix services github-actions-vm-mint))
                      (setenv "GUILE_EXTENSIONS_PATH"
                              #$(file-append guile-gnutls
                                             "/lib/guile/3.0/extensions"))
                      (match (cdr (command-line))
                        ((pat-file url token-file)
                         (catch #t
                           (lambda ()
                             (mint-and-store-registration-token
                              (read-pat-file pat-file) url token-file))
                           (lambda args
                             (format (current-error-port)
                                     "github-actions-runner: token minting failed: ~a~%"
                                     args)
                             (exit 1))))
                        (args
                         (format (current-error-port)
                                 "github-actions-runner: usage: ~a PAT-FILE URL TOKEN-FILE~%"
                                 "github-actions-runner-mint-registration-token")
                         (exit 2))))))))

(define-record-type* <github-actions-runner-token-configuration>
  github-actions-runner-token-configuration
  make-github-actions-runner-token-configuration
  github-actions-runner-token-configuration?
  (url        github-actions-runner-token-configuration-url)  ;string
  (pat-file   github-actions-runner-token-configuration-pat-file) ;string
  (token-file github-actions-runner-token-configuration-token-file
              (default "/var/lib/github-actions-runner/registration-token"))
  (runner-config-file
   github-actions-runner-token-configuration-runner-config-file
   (default "/var/lib/github-actions-runner/.runner"))
  (user       github-actions-runner-token-configuration-user
              (default "github-actions-runner"))
  (group      github-actions-runner-token-configuration-group
              (default "github-actions-runner"))
  (requirements github-actions-runner-token-configuration-requirements
                (default '()))                     ;list of shepherd symbols
  )

(define (github-actions-runner-token-mint-script config)
  "Return a Guile program, as a file-like object, that is the token
minting one-shot of CONFIG: exit successfully when the runner is
already registered, mint a fresh registration token from the PAT file
otherwise (reusing one younger than 55 minutes, see
`github-actions-runner-mint-program'), and hand the token file to the
runner's USER and GROUP.  The program depends on no ambient
environment, as Shepherd's clean service environment requires."
  (match-record config <github-actions-runner-token-configuration>
    (url pat-file token-file runner-config-file user group)
    ;; See `github-actions-runner-mint-program' for why guile-gnutls is
    ;; an extension and GUILE_EXTENSIONS_PATH is set explicitly.
    (program-file
     "github-actions-runner-token-mint"
     (with-extensions (list guile-json-4 guile-gnutls)
       (with-imported-modules '((guix build utils)
                                (r0man guix services github-actions-vm-mint))
         #~(begin
             (use-modules (guix build utils)
                          (r0man guix services github-actions-vm-mint))

             (define (log fmt . args)
               (apply format (current-error-port)
                      (string-append "github-actions-runner: " fmt "~%")
                      args))

             (setenv "GUILE_EXTENSIONS_PATH"
                     #$(file-append guile-gnutls
                                    "/lib/guile/3.0/extensions"))

             (mkdir-p (dirname #$runner-config-file))
             (mkdir-p (dirname #$token-file))

             ;; The runner's registered credentials do not expire, so
             ;; once it is registered there is nothing to mint.
             (when (file-exists? #$runner-config-file)
               (log "registered; not minting")
               (exit 0))
             (unless (access? #$pat-file R_OK)
               (log "PAT file ~a is not readable" #$pat-file)
               (exit 1))

             ;; The token expires one hour after minting, so it is
             ;; minted only when the runner is about to register, and
             ;; kept out of everyone's reach but the runner account's.
             (catch #t
               (lambda ()
                 (mint-and-store-registration-token
                  (read-pat-file #$pat-file) #$url #$token-file))
               (lambda args
                 (log "token minting failed: ~a" args)
                 (exit 1)))
             (chown #$token-file
                    (passwd:uid (getpwnam #$user))
                    (group:gid (getgrnam #$group)))))))))

(define (github-actions-runner-token-shepherd-service config)
  "Return the one-shot Shepherd service of CONFIG: mint a registration
token, once per boot, unless the runner is already registered."
  (shepherd-service
   (documentation "Mint a GitHub Actions runner registration token
(one hour validity) from a PAT, when the runner is not yet registered.")
   (provision '(github-actions-runner-token))
   ;; The runner service `require's this, so the runner does not start
   ;; until the token it needs exists (or the one-shot proves none was
   ;; needed).  The fields' own requirements (typically the one-shot
   ;; that decrypts the PAT) must be up first.
   (requirement `(user-processes networking
                  ,@(github-actions-runner-token-configuration-requirements
                     config)))
   (one-shot? #t)
   (start #~(lambda args
              (zero? (spawn-command
                      (list #$(github-actions-runner-token-mint-script
                               config))))))
   (stop #~(const #f))))

(define github-actions-runner-token-mint-service-type
  (service-type
   (name 'github-actions-runner-token-mint)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (lambda (config)
                               (list
                                (github-actions-runner-token-shepherd-service
                                 config))))))
   (default-value
     ;; No defaults for the two fields every deployment must state.
     (github-actions-runner-token-configuration
      (url #f) (pat-file #f)))
   (description
    "Mint GitHub Actions runner registration tokens from a personal
access token.  A one-shot Shepherd service that runs at every boot
where the runner configured by RUNNER-CONFIG-FILE is not yet
registered, mints a one-hour-valid registration token from the PAT at
PAT-FILE using the GitHub REST API, and writes it to TOKEN-FILE (mode
0600, owned by USER and GROUP) --- the file a
@code{github-actions-runner-service-type} whose @code{token} field
names the same TOKEN-FILE reads while it is unregistered.  Pair the two
service types to have a persistent runner register itself
automatically, with the PAT kept out of the runner account's reach.")))
