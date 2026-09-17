(define-module (r0man guix home services github-actions)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages bash)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix packages github-actions)
  #:use-module (r0man guix services github-actions)
  #:export (home-github-actions-runner-configuration
            home-github-actions-runner-configuration?
            home-github-actions-runner-service-type))

(define-record-type* <home-github-actions-runner-configuration>
  home-github-actions-runner-configuration
  make-home-github-actions-runner-configuration
  home-github-actions-runner-configuration?
  (package   home-github-actions-runner-configuration-package
             (default github-actions-runner))
  (work-dir  home-github-actions-runner-configuration-work-dir
             (default #f))            ;#f → $XDG_DATA_HOME/actions-runner
  (url       home-github-actions-runner-configuration-url
             (default #f))            ;string or #f
  (token     home-github-actions-runner-configuration-token
             (default #f))            ;string, file-like, or #f
  (name      home-github-actions-runner-configuration-name
             (default #f))            ;string or #f
  (labels    home-github-actions-runner-configuration-labels
             (default '()))           ;list of strings
  (replace?  home-github-actions-runner-configuration-replace?
             (default #f))
  (extra-registration-args
   home-github-actions-runner-configuration-extra-registration-args
   (default '()))                     ;list of strings
  (environment-variables
   home-github-actions-runner-configuration-environment-variables
   (default '())))                    ;list of "KEY=value" strings

(define (home-github-actions-runner-log-file config)
  ;; Path of the runner log file under $XDG_STATE_HOME.
  (string-append
   (or (getenv "XDG_STATE_HOME")
       (string-append (getenv "HOME") "/.local/state"))
   "/github-actions-runner.log"))

(define (home-github-actions-runner-shepherd-services config)
  (match-record config <home-github-actions-runner-configuration>
    (package work-dir url token name labels replace?
             extra-registration-args environment-variables)
    (let ((script
           (github-actions-runner-start-script
            #:package package
            ;; The default matches the default of the actions-runner and
            ;; actions-runner-config launchers, so that runner directories
            ;; are shared between manual and service runs.
            #:work-dir (or work-dir
                           "${XDG_DATA_HOME:-$HOME/.local/share}/actions-runner")
            #:url url
            #:token token
            #:name name
            #:labels labels
            #:replace? replace?
            #:extra-registration-args extra-registration-args
            #:environment-variables environment-variables)))
      (list (shepherd-service
             (documentation "Run a self-hosted GitHub Actions runner.")
             (provision '(github-actions-runner))
             (respawn? #t)
             (start #~(make-forkexec-constructor
                       (list #$(file-append bash-minimal "/bin/bash")
                             #$script)
                       #:log-file
                       #$(home-github-actions-runner-log-file config)))
             (stop #~(make-kill-destructor)))))))

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
   (name 'home-github-actions-runner)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-github-actions-runner-shepherd-services)
          (service-extension home-activation-service-type
                             home-github-actions-runner-activation)
          (service-extension home-profile-service-type
                             (lambda (config)
                               (list
                                (home-github-actions-runner-configuration-package
                                 config))))))
   (default-value (home-github-actions-runner-configuration))
   (description
    "Run a self-hosted GitHub Actions runner as a user-level Shepherd
service.  The runner is registered with GitHub on first start using the
@code{url} and @code{token} configuration fields, and executes jobs
dispatched by GitHub Actions workflows from @code{work-dir}, which
defaults to @file{$XDG_DATA_HOME/actions-runner}.")))
