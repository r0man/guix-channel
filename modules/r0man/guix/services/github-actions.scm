(define-module (r0man guix services github-actions)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
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
   (default #f))                                ;string path or #f
  (log-file
   github-actions-runner-configuration-log-file
   (default "/var/log/github-actions-runner.log")))

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
                ;; Full store paths throughout: shepherd runs the
                ;; script with a clean environment, where a bare
                ;; `mkdir' resolves to nothing.
                (file-append coreutils "/bin/mkdir") " -p \"$RUNNER_DIR\"\n"
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
                "        " (file-append coreutils "/bin/mkdir")
                " -p \"$(" (file-append coreutils "/bin/dirname")
                " \"$REGISTRATION_MARKER\")\"\n"
                "        touch \"$REGISTRATION_MARKER\" || true\n"
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
                       (list #$(file-append bash-minimal "/bin/bash")
                             #$script)
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
  "Return a file-like object holding the Bash script of the token
minting one-shot: exit when the runner is already registered, mint a
fresh registration token from the PAT file with
`github-actions-runner-mint-program' otherwise, and hand the token file
to the runner's USER and GROUP."
  (define token-file
    (github-actions-runner-token-configuration-token-file config))
  (mixed-text-file "github-actions-runner-token-mint"
                   "#!" (file-append bash-minimal "/bin/bash") "\n"
                   "set -eu\n"
                   "\n"
                   "RUNNER_CONFIG="
                   (shell-quote
                    (github-actions-runner-token-configuration-runner-config-file
                     config))
                   "\n"
                   "PAT_FILE="
                   (shell-quote
                    (github-actions-runner-token-configuration-pat-file
                     config))
                   "\n"
                   "TOKEN_FILE=" (shell-quote token-file) "\n"
                   "\n"
                   ;; Full store paths: shepherd runs the script with
                   ;; a clean environment, where bare `mkdir' and
                   ;; `dirname' resolve to nothing.
                   (file-append coreutils "/bin/mkdir")
                   " -p \"$(" (file-append coreutils "/bin/dirname")
                   " \"$RUNNER_CONFIG\")\"\n"
                   (file-append coreutils "/bin/mkdir")
                   " -p \"$(" (file-append coreutils "/bin/dirname")
                   " \"$TOKEN_FILE\")\"\n"
                   ;; The runner's registered credentials do not
                   ;; expire, so once it is registered there is nothing
                   ;; to mint.
                   "if [ -f \"$RUNNER_CONFIG\" ]; then\n"
                   "    echo \"github-actions-runner: registered; not minting\" >&2\n"
                   "    exit 0\n"
                   "fi\n"
                   "if [ ! -r \"$PAT_FILE\" ]; then\n"
                   "    echo \"github-actions-runner: PAT file $PAT_FILE is not readable\" >&2\n"
                   "    exit 1\n"
                   "fi\n"
                   ;; The token expires one hour after minting, so it
                   ;; is minted only when the runner is about to
                   ;; register, and kept out of everyone's reach but
                   ;; the runner account's.
                   (github-actions-runner-mint-program)
                   " \"$PAT_FILE\" "
                   (shell-quote
                    (github-actions-runner-token-configuration-url config))
                   " \"$TOKEN_FILE\"\n"
                   (file-append coreutils "/bin/chown")
                   " "
                   (github-actions-runner-token-configuration-user config)
                   ":"
                   (github-actions-runner-token-configuration-group config)
                   " \"$TOKEN_FILE\"\n"))

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
                      (list #$(file-append bash-minimal "/bin/bash")
                            #$(github-actions-runner-token-mint-script
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
