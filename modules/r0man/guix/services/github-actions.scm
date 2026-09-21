(define-module (r0man guix services github-actions)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (r0man guix packages github-actions)
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
            github-actions-runner-start-script
            github-actions-runner-service-type))

(define (shell-quote str)
  "Quote STR as a single word for interpolation into a POSIX shell
script."
  (string-append
   "\""
   (string-concatenate
    (map (lambda (ch)
           (if (memv ch '(#\" #\$ #\` #\\))
               (string #\\ ch)
               (string ch)))
         (string->list str)))
   "\""))

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
   (default #f)))                               ;string path or #f

(define* (github-actions-runner-start-script
          #:key
          (package github-actions-runner)
          (work-dir "${XDG_DATA_HOME:-$HOME/.local/share}/actions-runner")
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
  "Return a file-like object holding a Bash script that prepares the
writable runner work directory WORK-DIR (populated on first use by the
launchers of the actions-runner package), registers the runner with
GitHub unless it has been registered before, and finally execs the
runner.

TOKEN is either a registration token string, a file-like object holding
the token, an absolute file path holding the token, or #f.  WORK-DIR
and the values of ENVIRONMENT-VARIABLES are interpolated as-is into the
generated script, so they may contain shell variable references, but
must not contain double quotes or newlines.

When EPHEMERAL? is true, the script runs the runner without exec'ing it
(its exit status matters) and, when SHUTDOWN-FILE is set, touches that
file once the runner has exited, so that an external supervisor can
tear down the machine the runner lived in.  On failure (missing
credentials, or a failed registration), the script waits for 300
seconds before touching the shutdown file, which backs off respawning a
broken runner.  When REGISTRATION-MARKER is set, the script touches it
after a successful registration, which lets an external supervisor
expire credentials that are no longer needed.  All three fields exist
for the VM-backed runner service; the default values keep the ordinary
behavior: exec the runner and never touch any marker file.

SUPPLEMENTARY-GROUPS makes the runner account join additional groups
such as @code{docker}, and REQUIREMENTS adds shepherd services to the runner
service's requirement list (both are consumed by the shepherd service
and the accounts extensions, not by the start script)."
  (define registration-args
    (string-append
     "--unattended --url \"$URL\" --token \"$TOKEN\""
     (if name
         (string-append " --name " (shell-quote name))
         "")
     (if (null? labels)
         ""
         (string-append " --labels "
                        (shell-quote (string-join labels ","))))
     (if replace?
         " --replace"
         "")
     (if (null? extra-registration-args)
         ""
         (string-append " "
                        (string-join
                         (map shell-quote extra-registration-args)
                         " ")))))

  (define token-lines
    (cond ((not token)
           (list "TOKEN=\"\"\n"))
          ((file-like? token)
           (list "TOKEN=\"\"\n"
                 "if [ -f " token " ]; then\n"
                 "    TOKEN=\"$("
                 (file-append coreutils "/bin/tr")
                 " -d '\\r\\n' < " token ")\"\n"
                 "fi\n"))
          ((and (string? token) (string-prefix? "/" token))
           ;; An absolute file path holding the token, read at start
           ;; time.  Used by the VM-backed service, which mounts a seed
           ;; directory with a minted token into the runner VM.
           (list "TOKEN=\"\"\n"
                 "if [ -f " token " ]; then\n"
                 "    TOKEN=\"$("
                 (file-append coreutils "/bin/tr")
                 " -d '\\r\\n' < " token ")\"\n"
                 "fi\n"))
          (else
           (list "TOKEN=" (shell-quote token) "\n"))))

  (apply mixed-text-file
         "github-actions-runner-start"
         (append
          (list "#!" (file-append bash-minimal "/bin/bash") "\n"
                ;; Generated by (r0man guix services github-actions).
                ;; Do not edit.
                "set -eu\n"
                "\n"
                "RUNNER_DIR=\"" work-dir "\"\n"
                "export ACTIONS_RUNNER_DIR=\"$RUNNER_DIR\"\n"
                "export HOME=\"$RUNNER_DIR\"\n")
          (append-map
           (lambda (var)
             (list "export \"" var "\"\n"))
           environment-variables)
          (list "\n"
                "mkdir -p \"$RUNNER_DIR\"\n"
                "cd \"$RUNNER_DIR\"\n"
                "\n"
                "URL=" (shell-quote (or url "")) "\n"
                "SHUTDOWN_FILE=" (shell-quote (or shutdown-file "")) "\n"
                "REGISTRATION_MARKER="
                (shell-quote (or registration-marker "")) "\n")
          token-lines
          (list "\n"
                "CONFIG="
                (file-append package "/bin/actions-runner-config")
                "\n"
                "RUN="
                (file-append package "/bin/actions-runner")
                "\n"
                "\n"
                "if [ ! -f \"$RUNNER_DIR/.runner\" ]; then\n"
                "    if [ -z \"$URL\" ] || [ -z \"$TOKEN\" ]; then\n"
                "        echo \"github-actions-runner: $RUNNER_DIR is not registered with GitHub, and no\" >&2\n"
                "        echo \"github-actions-runner: URL or registration token was provided.\" >&2\n"
                "        echo \"github-actions-runner: Either set the 'url' and 'token' fields of\" >&2\n"
                "        echo \"github-actions-runner: github-actions-runner-configuration, or register the\" >&2\n"
                "        echo \"github-actions-runner: runner manually by running: $CONFIG\" >&2\n"
                "        if [ -n \"$SHUTDOWN_FILE\" ]; then\n"
                "            sleep 300\n"
                "            touch \"$SHUTDOWN_FILE\" || true\n"
                "        fi\n"
                "        exit 1\n"
                "    fi\n"
                "    echo \"github-actions-runner: registering runner in $RUNNER_DIR\"\n"
                "    if ! \"$CONFIG\" " registration-args "; then\n"
                "        echo \"github-actions-runner: registration failed\" >&2\n"
                "        if [ -n \"$SHUTDOWN_FILE\" ]; then\n"
                "            sleep 300\n"
                "            touch \"$SHUTDOWN_FILE\" || true\n"
                "        fi\n"
                "        exit 1\n"
                "    fi\n"
                "    if [ -n \"$REGISTRATION_MARKER\" ]; then\n"
                "        mkdir -p \"$(dirname \"$REGISTRATION_MARKER\")\"\n"
                "        touch \"$REGISTRATION_MARKER\"\n"
                "    fi\n"
                "fi\n")
          (if ephemeral?
              (list
               "\n"
               "set +e\n"
               "\"$RUN\"\n"
               "status=$?\n"
               "set -e\n"
               "echo \"github-actions-runner: runner exited with status $status\"\n"
               "if [ -n \"$SHUTDOWN_FILE\" ]; then\n"
               "    if [ \"$status\" -ne 0 ]; then\n"
               "        sleep 300\n"
               "    fi\n"
               "    touch \"$SHUTDOWN_FILE\" || true\n"
               "fi\n"
               "exit \"$status\"\n")
              (list "\n"
                    "exec \"$RUN\"\n")))))

(define (github-actions-runner-shepherd-service config)
  (match-record config <github-actions-runner-configuration>
    (package user group work-dir url token name labels replace?
             extra-registration-args environment-variables
             supplementary-groups ephemeral? requirements
             shutdown-file registration-marker)
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
             (start #~(make-forkexec-constructor
                       (list #$(file-append bash-minimal "/bin/bash")
                             #$script)
                       #:user #$user
                       #:group #$group
                       #:log-file "/var/log/github-actions-runner.log"))
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
