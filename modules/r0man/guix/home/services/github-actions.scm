;;; Home service counterpart of (r0man guix services github-actions).
;;;
;;; The service type is derived from the system service type with
;;; 'system->home-service-type', following the convention of upstream
;;; services such as (gnu home services syncthing) and (gnu home
;;; services mcron).  Like the system service, its value is a list of
;;; runner configurations, one per instance, so that a user can run
;;; several runners in parallel.

(define-module (r0man guix home services github-actions)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (r0man guix services github-actions)
  #:use-module (srfi srfi-1)
  #:export (home-github-actions-runner-service-type
            home-github-actions-runner-log-file)
  #:re-export (github-actions-runner-configuration
               github-actions-runner-configuration?
               github-actions-runner-configuration-package
               github-actions-runner-configuration-id
               github-actions-runner-configuration-instances
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
               github-actions-runner-configurations
               github-actions-runner-provision))

(define (home-github-actions-runner-state-directory)
  ;; $XDG_STATE_HOME, resolved when the Home environment is built.
  (or (getenv "XDG_STATE_HOME")
      (string-append (getenv "HOME") "/.local/state")))

(define (home-github-actions-runner-log-file config)
  "Return the log file of the runner instance CONFIG in a Home
environment: its `log-file' field, or a file under $XDG_STATE_HOME
named after its id."
  (or (github-actions-runner-configuration-log-file config)
      (let ((id (github-actions-runner-configuration-id config)))
        (string-append (home-github-actions-runner-state-directory)
                       "/github-actions-runner"
                       (if id (string-append "-" id) "")
                       ".log"))))

(define (home-github-actions-runner-shepherd-service config)
  ;; Plain accessors instead of 'match-record': the record type is
  ;; defined in (r0man guix services github-actions), and a literal
  ;; <github-actions-runner-configuration> type argument to 'match-record'
  ;; is not resolvable while compiling this module.
  (define id
    (github-actions-runner-configuration-id config))

  (define script
    (github-actions-runner-start-script
     #:package (github-actions-runner-configuration-package config)
     #:id id
     ;; #f, the default, is resolved by the start program from
     ;; XDG_DATA_HOME or HOME when the runner starts.
     #:work-dir (github-actions-runner-configuration-work-dir config)
     #:url (github-actions-runner-configuration-url config)
     #:token (github-actions-runner-configuration-token config)
     #:name (github-actions-runner-configuration-name config)
     #:labels (github-actions-runner-configuration-labels config)
     #:replace? (github-actions-runner-configuration-replace? config)
     #:extra-registration-args
     (github-actions-runner-configuration-extra-registration-args
      config)
     #:environment-variables
     (github-actions-runner-configuration-environment-variables
      config)))

  ;; Unlike the system service, the runner runs as the user that owns
  ;; the Home environment: no #:user and #:group are passed to
  ;; 'make-forkexec-constructor', and there is no 'networking or
  ;; 'user-processes requirement, which only exist in the system
  ;; Shepherd.
  (shepherd-service
   (documentation
    (if id
        (string-append "Run the self-hosted GitHub Actions runner '" id
                       "' as a user service.")
        "Run a self-hosted GitHub Actions runner as a user service."))
   (provision (list (github-actions-runner-provision config)))
   (requirement (github-actions-runner-configuration-requirements config))
   (respawn? #t)
   (start #~(make-forkexec-constructor
             (list #$script)
             #:log-file
             #$(home-github-actions-runner-log-file config)))
   (stop #~(make-kill-destructor))))

(define (home-github-actions-runner-shepherd-services value)
  (map home-github-actions-runner-shepherd-service
       (github-actions-runner-configurations value)))

(define (home-github-actions-runner-activation value)
  ;; Make sure the directory of the log files exists before the shepherd
  ;; services start.
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p #$(home-github-actions-runner-state-directory))))

(define (home-github-actions-runner-profile value)
  (delete-duplicates
   (map github-actions-runner-configuration-package
        (github-actions-runner-configurations value))))

(define home-github-actions-runner-service-type
  (service-type
   (inherit (system->home-service-type github-actions-runner-service-type))
   ;; 'system->home-service-type' alone is not enough: it has no mapping
   ;; for 'account-service-type' (a Home environment has no system
   ;; accounts), the system Shepherd extension hardcodes a dedicated
   ;; runner user and group plus /var/log log files, and the system
   ;; activation extension chowns directories under /var/lib.  Redefine
   ;; the extensions instead; the runner work directories are created by
   ;; the start script itself.
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-github-actions-runner-shepherd-services)
          (service-extension home-activation-service-type
                             home-github-actions-runner-activation)
          (service-extension home-profile-service-type
                             home-github-actions-runner-profile)))
   (default-value
     ;; One instance without id, whose work directory the start program
     ;; resolves from XDG_DATA_HOME or HOME, matching the default of the
     ;; actions-runner and actions-runner-config launchers, so that
     ;; runner directories are shared between manual and service runs.
     ;; The 'user' and 'group' fields of the configuration are ignored:
     ;; the runner runs as the user that owns the Home environment.
     (list (github-actions-runner-configuration)))
   (description
    "Run self-hosted GitHub Actions runners as user-level Shepherd
services.  This is the Home counterpart of
@code{github-actions-runner-service-type}: the value is a list of
@code{github-actions-runner-configuration} records, one per runner
instance, and the runners run as the user that owns the Home
environment, with their work directories defaulting to
@file{$XDG_DATA_HOME/actions-runner} (@file{actions-runner-@var{id}}
for an instance with an id) and their log files under
@file{$XDG_STATE_HOME}.  Each instance is registered with GitHub on
first start using its @code{url} and @code{token} fields, and executes
jobs dispatched by GitHub Actions workflows from its work directory.")))

;; Allow a Home configuration that contains a system service
;; configuration for the runner to be automatically mapped to the Home
;; service, as (gnu home services mcron) does.
(define-service-type-mapping
  github-actions-runner-service-type => home-github-actions-runner-service-type)
