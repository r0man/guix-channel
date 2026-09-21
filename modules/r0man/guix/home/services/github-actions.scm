;;; Home service counterpart of (r0man guix services github-actions).
;;;
;;; The service type is derived from the system service type with
;;; 'system->home-service-type', following the convention of upstream
;;; services such as (gnu home services syncthing) and (gnu home
;;; services mcron).

(define-module (r0man guix home services github-actions)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages bash)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (r0man guix services github-actions)
  #:export (home-github-actions-runner-service-type)
  #:re-export (github-actions-runner-configuration
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
               github-actions-runner-configuration-registration-marker))

(define (home-github-actions-runner-log-file config)
  ;; Path of the runner log file under $XDG_STATE_HOME.
  (string-append
   (or (getenv "XDG_STATE_HOME")
       (string-append (getenv "HOME") "/.local/state"))
   "/github-actions-runner.log"))

(define (home-github-actions-runner-shepherd-service config)
  ;; Plain accessors instead of 'match-record': the record type is
  ;; defined in (r0man guix services github-actions), and a literal
  ;; <github-actions-runner-configuration> type argument to 'match-record'
  ;; is not resolvable while compiling this module.
  (let ((script
         (github-actions-runner-start-script
          #:package (github-actions-runner-configuration-package config)
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
           config))))
    ;; Unlike the system service, the runner runs as the user that owns
    ;; the Home environment: no #:user and #:group are passed to
    ;; 'make-forkexec-constructor', and there is no 'networking or
    ;; 'user-processes requirement, which only exist in the system
    ;; Shepherd.
    (list (shepherd-service
           (documentation
            "Run a self-hosted GitHub Actions runner as a user service.")
           (provision '(github-actions-runner))
           (respawn? #t)
           (start #~(make-forkexec-constructor
                     (list #$(file-append bash-minimal "/bin/bash")
                           #$script)
                     #:log-file
                     #$(home-github-actions-runner-log-file config)))
           (stop #~(make-kill-destructor))))))

(define (home-github-actions-runner-activation config)
  ;; Make sure the directory of the log file exists before the shepherd
  ;; service starts.
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p
       (dirname
        #$(home-github-actions-runner-log-file config)))))

(define home-github-actions-runner-service-type
  (service-type
   (inherit (system->home-service-type github-actions-runner-service-type))
   ;; 'system->home-service-type' alone is not enough: it has no mapping
   ;; for 'account-service-type' (a Home environment has no system
   ;; accounts), the system Shepherd extension hardcodes a dedicated
   ;; runner user and group plus a /var/log log file, and the system
   ;; activation extension chowns directories under /var/lib.  Redefine
   ;; the extensions instead; the runner work directory is created by
   ;; the start script itself.
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-github-actions-runner-shepherd-service)
          (service-extension home-activation-service-type
                             home-github-actions-runner-activation)
          (service-extension home-profile-service-type
                             (lambda (config)
                               (list
                                (github-actions-runner-configuration-package
                                 config))))))
   (default-value
     (for-home
      (github-actions-runner-configuration
       ;; Matches the default of the actions-runner and
       ;; actions-runner-config launchers, so that runner directories
       ;; are shared between manual and service runs.  The 'user' and
       ;; 'group' fields of the configuration are ignored: the runner
       ;; runs as the user that owns the Home environment.
       (work-dir "${XDG_DATA_HOME:-$HOME/.local/share}/actions-runner"))))
   (description
    "Run a self-hosted GitHub Actions runner as a user-level Shepherd
service.  This is the Home counterpart of
@code{github-actions-runner-service-type}: the runner runs as the user
that owns the Home environment, with its work directory defaulting to
@file{$XDG_DATA_HOME/actions-runner} and its log file under
@file{$XDG_STATE_HOME}.  The runner is registered with GitHub on first
start using the @code{url} and @code{token} configuration fields, and
executes jobs dispatched by GitHub Actions workflows from
@code{work-dir}.")))

;; Allow a Home configuration that contains a system service
;; configuration for the runner to be automatically mapped to the Home
;; service, as (gnu home services mcron) does.
(define-service-type-mapping
  github-actions-runner-service-type => home-github-actions-runner-service-type)
