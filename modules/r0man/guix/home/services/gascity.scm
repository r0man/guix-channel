;;; Home service counterpart of (r0man guix services gascity).
;;;
;;; The service type is derived from the system service type, following the
;;; convention of upstream services such as (gnu home services syncthing) and
;;; (gnu home services mcron).  Like the system service, its value is a list
;;; of supervisor configurations, one per user-level supervisor process.
;;;
;;; As github-actions documents (@16.4), 'system->home-service-type' alone is
;;; not enough: the derived type keeps the system 'account-service-type'
;;; extension (a Home environment has no system accounts) and its system
;;; Shepherd and activation extensions hardcode a dedicated user and /var/...
;;; paths.  This module redefines the extensions explicitly.

(define-module (r0man guix home services gascity)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (r0man guix services gascity)
  #:use-module (srfi srfi-1)
  #:export (home-gascity-service-type
            home-gascity-state-directory
            home-gascity-home-directory
            home-gascity-log-directory
            home-gascity-gc-home
            home-gascity-supervisor-configuration
            home-gascity-supervisor-configurations
            home-gascity-supervisor-provision
            home-gascity-supervisor-shepherd-service-name)
  )

;;;
;;; Home path defaults.
;;;
;;; The system service derives its paths from the account's home under
;;; /var/lib.  The Home service derives them from the user's environment
;;; instead, resolved when the Home environment is built: the state and log
;;; directories under $XDG_STATE_HOME (or $HOME/.local/state), and GC_HOME
;;; under $HOME, matching the interactive `gc' default.

(define (home-gascity-state-home)
  ;; $XDG_STATE_HOME, resolved when the Home environment is built.
  (or (getenv "XDG_STATE_HOME")
      (string-append (getenv "HOME") "/.local/state")))

(define (home-gascity-state-directory)
  "Return the state directory of the Home Gas City service:
$XDG_STATE_HOME/gascity, or $HOME/.local/state/gascity."
  (string-append (home-gascity-state-home) "/gascity"))

(define (home-gascity-home-directory)
  "Return the HOME of the Home Gas City service: the real home of the user
who owns the Home environment, so a user-level supervisor sees that user's own
tools, git configuration and SSH/GitHub credentials.  An explicit
`home-directory' on the supervisor configuration overrides it."
  (getenv "HOME"))

(define (home-gascity-log-directory)
  "Return the log directory of the Home Gas City service: the state
directory."
  (home-gascity-state-directory))

(define (home-gascity-gc-home)
  "Return the default GC_HOME of the Home Gas City service: $HOME/.gc, the
registry, settings, control socket and instance lock of a user supervisor."
  (string-append (getenv "HOME") "/.gc"))


;;;
;;; Configuration normalization.
;;;

(define (home-gascity-supervisor-configuration config)
  "Return the <gascity-supervisor-configuration> CONFIG with the Home path
defaults filled in: `state-directory' and `log-directory' default to
$XDG_STATE_HOME/gascity, `home-directory' to $HOME (the user's real home) and
`gc-home' to $HOME/.gc.  The `user', `group' and `create-account?' fields are
ignored: a Home environment has no system accounts, and the supervisor runs as
the user that owns it."
  (gascity-supervisor-configuration
   (inherit config)
   (state-directory
    (let ((directory (gascity-supervisor-configuration-state-directory config)))
      (if (eq? directory 'unset) (home-gascity-state-directory) directory)))
   (home-directory
    (let ((directory (gascity-supervisor-configuration-home-directory config)))
      (if (eq? directory 'unset) (home-gascity-home-directory) directory)))
   (log-directory
    (let ((directory (gascity-supervisor-configuration-log-directory config)))
      (if (eq? directory 'unset) (home-gascity-log-directory) directory)))
   (gc-home
    (let ((directory (gascity-supervisor-configuration-gc-home config)))
      (if (eq? directory 'unset) (home-gascity-gc-home) directory)))))

(define (home-gascity-supervisor-configurations value)
  "Return the normalized, home-defaulted supervisor configurations of the
Home service VALUE, validating it as the system service does.  The paths of
each configuration default to the Home paths; an invalid VALUE is passed on
unchanged so that the system validation raises the same error."
  (gascity-supervisor-configurations
   (if (and (list? value)
            (every gascity-supervisor-configuration? value))
       (map home-gascity-supervisor-configuration value)
       value)))


;;;
;;; Shepherd service names.
;;;

(define (home-gascity-supervisor-provision config)
  "Return the Home Shepherd service name of the provision one-shot of the
supervisor instance CONFIG: `home-gascity-provision', suffixed with its id."
  (let ((id (gascity-supervisor-configuration-id config)))
    (if (and (string? id) (not (string-null? id)))
        (string->symbol (string-append "home-gascity-provision-" id))
        'home-gascity-provision)))

(define (home-gascity-supervisor-shepherd-service-name config)
  "Return the Home Shepherd service name of the supervisor instance CONFIG:
`home-gascity-supervisor', suffixed with its id."
  (let ((id (gascity-supervisor-configuration-id config)))
    (if (and (string? id) (not (string-null? id)))
        (string->symbol (string-append "home-gascity-supervisor-" id))
        'home-gascity-supervisor)))


;;;
;;; Shepherd services.
;;;

(define (home-gascity-supervisor-provision-shepherd-service config)
  "Return the one-shot Home Shepherd service of the supervisor instance
CONFIG: it runs the shared provision program as the current user and waits
for it.  There is no `#:user'/`#:group' and no `networking'/`user-processes'
requirement: those only exist in the system Shepherd."
  (shepherd-service
   (documentation
    "Provision a Gas City city as a user service: genesis, site binding, \
packs and registry.")
   (provision (list (home-gascity-supervisor-provision config)))
   (requirement
    (gascity-supervisor-configuration-shepherd-requirement config))
   (one-shot? #t)
   (start #~(lambda args
              (zero? (spawn-command
                      (list #$(gascity-supervisor-provision-program
                               config))))))
   (stop #~(const #f))))

(define (home-gascity-supervisor-shepherd-service config)
  "Return the long-running Home Shepherd service of the supervisor instance
CONFIG: the shared secrets-loading wrapper that execs `gc supervisor run' as
the current user, with no `#:user'/`#:group' and no networking requirement."
  (let ((home (gascity-supervisor-state-directory config)))
    (shepherd-service
     (documentation
      "Run a Gas City supervisor as a user service (gc supervisor run).")
     (provision (list (home-gascity-supervisor-shepherd-service-name config)))
     (requirement
      (cons (home-gascity-supervisor-provision config)
            (gascity-supervisor-configuration-shepherd-requirement config)))
     (start #~(make-forkexec-constructor
               (list #$(gascity-supervisor-program config))
               #:directory #$home
               #:log-file #$(gascity-supervisor-log-file config)
               #:environment-variables
               #$(gascity-supervisor-environment config)))
     (stop #~(make-kill-destructor))
     (respawn? #t))))

(define (home-gascity-supervisor-shepherd-services value)
  "Return the Home Shepherd services of the supervisor instances of VALUE:
for each instance, the provision one-shot followed by the supervisor, which
requires it."
  (append-map (lambda (config)
                (list (home-gascity-supervisor-provision-shepherd-service
                       config)
                      (home-gascity-supervisor-shepherd-service config)))
              (home-gascity-supervisor-configurations value)))


;;;
;;; Activation and profile.
;;;

(define (home-gascity-supervisor-activation value)
  "Return the Home activation gexp of VALUE: create each instance's state
directory and materialize its generated files for the user that owns the
Home environment.  Unlike the system activation it does not chown and does
not create the service account."
  (let* ((configs (home-gascity-supervisor-configurations value))
         (directories
          (map gascity-supervisor-state-directory configs)))
    #~(begin
        (use-modules (guix build utils))
        (for-each mkdir-p '#$directories)
        #$@(map (lambda (config)
                  #~(invoke
                     #$(gascity-supervisor-activation-program
                        config #:chown? #f)))
                configs))))

(define (home-gascity-supervisor-profile value)
  "Return the packages of the supervisor instances of VALUE."
  (delete-duplicates
   (map gascity-supervisor-configuration-package
        (home-gascity-supervisor-configurations value))))


;;;
;;; Service type.
;;;

(define home-gascity-service-type
  (service-type
   (inherit (system->home-service-type gascity-service-type))
   ;; 'system->home-service-type' alone is not enough: the derived type has no
   ;; mapping for 'account-service-type' (a Home environment has no system
   ;; accounts), and the system Shepherd and activation extensions hardcode a
   ;; dedicated user and /var/... paths.  Redefine the extensions explicitly,
   ;; as github-actions does (@16.4).  The system extensions dropped on
   ;; purpose are 'account-service-type' (no system accounts), the system
   ;; shepherd-root and activation extensions (they run as root and under
   ;; /var), and the system profile extension; they are replaced by the Home
   ;; shepherd, activation and profile extensions below.
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-gascity-supervisor-shepherd-services)
          (service-extension home-activation-service-type
                             home-gascity-supervisor-activation)
          (service-extension home-profile-service-type
                             home-gascity-supervisor-profile)))
   (default-value
     ;; One instance without id, whose state directory, log directory and
     ;; GC_HOME default to the Home paths.
     (list (home-gascity-supervisor-configuration
            (gascity-supervisor-configuration))))
   (description
    "Run Gas City supervisors (gc supervisor run) as user-level Shepherd
services.  This is the Home counterpart of @code{gascity-service-type}: the
value is a list of @code{gascity-supervisor-configuration} records, one per
supervisor process, and the supervisors run as the user that owns the Home
environment.  Each instance has its own @code{gc-home} (the registry,
settings, control socket and instance lock live under it) and its own API
@code{port}, so several supervisors coexist: a lone instance defaults to port
8372, and every instance of a multi-instance service must set @code{port}
explicitly.  The state and log directories default to
@file{$XDG_STATE_HOME/gascity} and @code{gc-home} to @file{$HOME/.gc}.  A
paired one-shot Shepherd service provisions each instance's cities (genesis,
site binding, optional pack fetching and the @file{cities.toml} registry)
before the supervisor starts.  Use @code{gascity-service-config} to configure
the common single-instance case, and pass @code{'()} when only extending a
configured service with further instances.")))

;; Allow a Home configuration that contains a system service configuration
;; for Gas City to be mapped to the Home service, as (gnu home services
;; mcron) does.
(define-service-type-mapping
  gascity-service-type => home-gascity-service-type)

;;;
;;; Re-exports.
;;;
;;; A Home configuration should need one import: this module.  The record
;;; surface (`gascity-city-configuration', `gascity-agent-configuration', the
;;; serializers, ...) therefore has to be re-exported.  The list is derived
;;; from `(r0man guix services gascity)' itself rather than maintained by
;;; hand: the hand-written list had grown to ~700 names and every new record
;;; or accessor silently stayed invisible to Home configurations until
;;; someone remembered to add it here.

(eval-when (expand load eval)
  (module-re-export!
   (current-module)
   (module-map
    (lambda (symbol variable) symbol)
    (module-public-interface
     (resolve-module '(r0man guix services gascity))))))
