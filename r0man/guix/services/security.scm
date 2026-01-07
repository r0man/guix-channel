(define-module (r0man guix services security)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (r0man guix packages security)
  #:export (cortex-agent-configuration
            cortex-agent-configuration?
            cortex-agent-configuration-package
            cortex-agent-configuration-config-file
            cortex-agent-configuration-distribution-id
            cortex-agent-configuration-distribution-server
            cortex-agent-configuration-endpoint-tags
            cortex-agent-configuration-proxy-list
            cortex-agent-configuration-unprivileged-user
            cortex-agent-configuration-kernel-module?
            cortex-agent-configuration-temporary-session?
            cortex-agent-configuration-vm-template?
            cortex-agent-configuration-restrictions
            cortex-agent-configuration-extra-options
            cortex-agent-configuration->arguments
            cortex-agent-generate-config-file
            cortex-agent-service-type))

;;; Commentary:
;;;
;;; Cortex XDR Agent service for Guix System.
;;; Provides endpoint security including Anti-Virus, EDR, Ransomware
;;; Protection, and Exploit Prevention.
;;;
;;; Code:

(define-record-type* <cortex-agent-configuration>
  cortex-agent-configuration make-cortex-agent-configuration
  cortex-agent-configuration?
  (package          cortex-agent-configuration-package
                    (default cortex-agent))
  (config-file      cortex-agent-configuration-config-file
                    (default #f)
                    (sanitizer
                     (lambda (value)
                       (if (or (not value) (file-like? value))
                           value
                           (raise (formatted-message
                                   (G_ "config-file must be a file-like \
object or #f")))))))
  (distribution-id  cortex-agent-configuration-distribution-id
                    (default ""))
  (distribution-server cortex-agent-configuration-distribution-server
                       (default ""))
  (endpoint-tags    cortex-agent-configuration-endpoint-tags
                    (default '()))
  (proxy-list       cortex-agent-configuration-proxy-list
                    (default '()))
  (unprivileged-user cortex-agent-configuration-unprivileged-user
                     (default "cortexuser"))
  (kernel-module?   cortex-agent-configuration-kernel-module?
                    (default #t))
  (temporary-session? cortex-agent-configuration-temporary-session?
                      (default #f))
  (vm-template?     cortex-agent-configuration-vm-template?
                    (default #f))
  (restrictions     cortex-agent-configuration-restrictions
                    (default '()))
  (extra-options    cortex-agent-configuration-extra-options
                    (default '())))

(define (cortex-agent-configuration->arguments config)
  "Convert a cortex-agent-configuration to command-line arguments."
  (define (maybe-add condition arg)
    (if condition (list arg) '()))

  (define (list-arg option values)
    (if (null? values)
        '()
        (list option (string-join values ","))))

  (append
   ;; Distribution settings
   (if (not (string-null?
             (cortex-agent-configuration-distribution-id config)))
       (list "--distribution-id"
             (cortex-agent-configuration-distribution-id config))
       '())
   (if (not (string-null?
             (cortex-agent-configuration-distribution-server config)))
       (list "--distribution-server"
             (cortex-agent-configuration-distribution-server config))
       '())

   ;; Tags and proxy
   (list-arg "--endpoint-tags"
             (cortex-agent-configuration-endpoint-tags config))
   (list-arg "--proxy-list"
             (cortex-agent-configuration-proxy-list config))

   ;; User and kernel module
   (list "--unprivileged-user"
         (cortex-agent-configuration-unprivileged-user config))
   (maybe-add (not (cortex-agent-configuration-kernel-module? config))
              "--no-km")

   ;; Session type
   (maybe-add (cortex-agent-configuration-temporary-session? config)
              "--temporary-session")
   (maybe-add (cortex-agent-configuration-vm-template? config)
              "--vm-template")

   ;; Restrictions
   (append-map (lambda (restriction)
                 (list "--restrict" restriction))
               (cortex-agent-configuration-restrictions config))

   ;; Extra options
   (cortex-agent-configuration-extra-options config)))

(define (cortex-agent-generate-config-file config)
  "Generate the cortex.conf file content from configuration."
  (let ((args (cortex-agent-configuration->arguments config)))
    (if (null? args)
        #f
        (plain-file "cortex.conf"
                    (string-join args "\n")))))

(define (cortex-agent-accounts config)
  "Return the user accounts needed for Cortex XDR Agent."
  (let ((user (cortex-agent-configuration-unprivileged-user config)))
    (list (user-account
           (name user)
           (group "nogroup")
           (system? #t)
           (comment "Cortex XDR Agent unprivileged user")
           (home-directory "/var/empty")
           (shell (file-append shadow "/sbin/nologin"))))))

(define (cortex-agent-activation config)
  "Return the activation gexp for Cortex XDR Agent."
  (let ((config-file (or (cortex-agent-configuration-config-file config)
                        (cortex-agent-generate-config-file config)))
        (package (cortex-agent-configuration-package config)))
    #~(begin
        (use-modules (guix build utils))

        ;; Create configuration directories
        (mkdir-p "/etc/panw")
        (mkdir-p "/etc/traps")
        (mkdir-p "/var/log/traps")
        (mkdir-p "/var/log/traps/agent")

        ;; Create /opt/traps base directory
        (mkdir-p "/opt/traps")

        ;; Symlink static content from the package to /opt/traps
        ;; This allows the agent to find its files at the expected paths
        (let ((pkg-traps (string-append #$package "/opt/traps")))
          (for-each
           (lambda (subdir)
             (let ((src (string-append pkg-traps "/" subdir))
                   (dst (string-append "/opt/traps/" subdir)))
               (when (file-exists? src)
                 ;; Remove existing symlink or empty directory
                 (when (file-exists? dst)
                   (if (symbolic-link? dst)
                       (delete-file dst)
                       (let ((contents (and (directory-exists? dst)
                                           (scandir dst
                                             (lambda (f)
                                               (not (member f '("." ".."))))))))
                        (when (and contents (null? contents))
                          (rmdir dst)))))
                 ;; Create symlink if destination doesn't exist
                 (unless (file-exists? dst)
                   (symlink src dst)))))
           ;; Static directories to symlink (read-only is fine)
           '("analyzerd" "apparmor" "bin" "ecl" "glibc"
             "init" "init.d" "kms" "km_utils"
             "policy" "rpm-installer" "scripts" "selinux" "systemd"
             "version.txt")))

        ;; Copy directories that need to be writable
        ;; - lib/lib32: lock files
        ;; - config: FIPS module generates fipsmodule.cnf at runtime
        (for-each
         (lambda (dir-name)
           (let ((src-dir (string-append #$package "/opt/traps/" dir-name))
                 (dst-dir (string-append "/opt/traps/" dir-name)))
             (when (and (file-exists? src-dir)
                        (not (file-exists? dst-dir)))
               (copy-recursively src-dir dst-dir)
               ;; Make config files writable so FIPS module can be generated
               (when (string=? dir-name "config")
                 (chmod dst-dir #o755)))))
         '("lib" "lib32" "config"))

        ;; Generate FIPS module config file if it doesn't exist
        ;; Must use the bootstrap config (openssl-fipsinstall.cnf) which
        ;; doesn't include the not-yet-existing fipsmodule.cnf
        ;; IMPORTANT: Fork to isolate environment variables
        ;; (LD_LIBRARY_PATH would break Guile/Shepherd if leaked)
        (let ((fips-config "/opt/traps/config/fipsmodule.cnf")
              (glibc-lib "/opt/traps/glibc/lib/x86_64-linux-gnu"))
          (unless (file-exists? fips-config)
            (let ((ld-linux (string-append glibc-lib "/ld-linux-x86-64.so.2")))
              (when (file-exists? ld-linux)
                ;; Fork and exec to isolate environment changes
                (let ((pid (primitive-fork)))
                  (cond
                   ((zero? pid)
                    ;; Child process: set env and exec
                    (setenv "OPENSSL_CONF"
                            "/opt/traps/config/openssl-fipsinstall.cnf")
                    (setenv "LD_LIBRARY_PATH"
                            (string-append glibc-lib ":/opt/traps/lib"))
                    (execl ld-linux ld-linux
                           "/opt/traps/bin/openssl"
                           "fipsinstall"
                           "-no_conditional_errors"
                           "-module" "/opt/traps/lib/fips.so"
                           "-out" fips-config)
                    (primitive-exit 1))
                   (else
                    ;; Parent: wait for child
                    (waitpid pid))))
                ;; Set appropriate permissions
                (when (file-exists? fips-config)
                  (chmod fips-config #o644))))))

        ;; Create directories the agent expects (Guix doesn't have cron by default)
        (for-each mkdir-p
                  '("/etc/cron.d"
                    "/var/spool/cron/crontabs"))

        ;; Create CA certificate symlinks for RHEL/CentOS-style paths
        ;; The agent looks for certificates at distribution-specific locations
        (let ((guix-ca-bundle "/etc/ssl/certs/ca-certificates.crt"))
          (when (file-exists? guix-ca-bundle)
            ;; Create /etc/pki structure for RHEL-style lookup
            (mkdir-p "/etc/pki/tls/certs")
            (unless (file-exists? "/etc/pki/tls/certs/ca-bundle.crt")
              (symlink guix-ca-bundle "/etc/pki/tls/certs/ca-bundle.crt"))
            ;; Also create alternate location
            (unless (file-exists? "/etc/ssl/ca-bundle.pem")
              (symlink guix-ca-bundle "/etc/ssl/ca-bundle.pem"))))

        ;; Create FHS-compatible directory structure for scripts expecting
        ;; standard Linux paths (/bin, /sbin, /usr/bin, /usr/sbin)
        (for-each mkdir-p '("/bin" "/sbin" "/usr/bin" "/usr/sbin"))

        ;; Symlink bash to /bin/bash for scripts that expect it
        (when (and (not (file-exists? "/bin/bash"))
                   (false-if-exception (readlink "/bin/bash")))
          (delete-file "/bin/bash"))
        (unless (file-exists? "/bin/bash")
          (symlink (string-append #$(file-append (@ (gnu packages bash) bash)
                                                  "/bin/bash"))
                   "/bin/bash"))

        ;; Symlink common utilities to FHS locations
        (let ((profile "/run/current-system/profile"))
          (for-each
           (lambda (cmd)
             (let ((src (string-append profile "/bin/" cmd))
                   (dst (string-append "/usr/bin/" cmd)))
               (when (and (file-exists? src)
                          (not (file-exists? dst)))
                 (symlink src dst))))
           '("env" "sh" "cat" "grep" "sed" "awk" "id" "whoami"
             "uname" "hostname" "date" "ls" "cp" "mv" "rm" "mkdir"
             "chmod" "chown" "ps" "kill" "sleep" "head" "tail" "wc"
             "sort" "uniq" "cut" "tr" "tee" "xargs" "find" "which")))

        ;; Create writable directories (these remain as real directories)
        (for-each mkdir-p
                  '("/opt/traps/persist"
                    "/opt/traps/download"
                    "/opt/traps/upload"
                    "/opt/traps/tmp"
                    "/opt/traps/ipc"
                    "/opt/traps/ipc_agent"
                    "/opt/traps/forensics"
                    "/opt/traps/edr"
                    "/opt/traps/fim"
                    "/opt/traps/trace"
                    "/opt/traps/quarantine"))

        ;; Copy or generate config file
        #$@(if config-file
               #~((let ((target "/etc/panw/cortex.conf"))
                    (copy-file #$config-file target)
                    (chown target 0 0)
                    (chmod target #o600)))
               #~()))))

(define (cortex-agent-shepherd-service config)
  "Return a Shepherd service for Cortex XDR Agent."
  (let ((package (cortex-agent-configuration-package config)))
    (list (shepherd-service
           (documentation "Palo Alto Networks Cortex XDR Agent daemon.")
           (provision '(cortex-agent traps-pmd))
           (requirement '(networking file-systems))
           (start #~(make-forkexec-constructor
                     ;; Run from /opt/traps which is symlinked to the package
                     (list "/opt/traps/bin/pmd")
                     #:log-file "/var/log/traps/pmd.log"
                     #:directory "/opt/traps"
                     #:environment-variables
                     ;; Include FHS paths for scripts that expect them
                     (list "LD_LIBRARY_PATH=/opt/traps/lib:/opt/traps/glibc/lib"
                           (string-append
                            "PATH=/opt/traps/bin"
                            ":/run/current-system/profile/bin"
                            ":/run/current-system/profile/sbin"
                            ":/bin:/sbin:/usr/bin:/usr/sbin")
                           "TRAPS_HOME=/opt/traps"
                           ;; SSL/TLS certificate environment variables
                           "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"
                           "SSL_CERT_DIR=/etc/ssl/certs"
                           "CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt"
                           "REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt")))
           (stop #~(lambda (pid)
                     ;; First stop the pmd process
                     (kill pid SIGTERM)
                     (sleep 2)
                     ;; Run km_manage stop if it exists
                     (when (file-exists? "/opt/traps/km_utils/km_manage")
                       (system* "/opt/traps/km_utils/km_manage" "stop"))
                     #f))
           (respawn? #t)
           (actions
            (list (shepherd-action
                   (name 'status)
                   (documentation "Check status of Cortex XDR agent.")
                   (procedure
                    #~(lambda (pid)
                        (if pid
                            (format #t "Cortex XDR agent running (PID ~a)~%"
                                   pid)
                            (format #t "Cortex XDR agent not running~%"))
                        #t)))))))))

(define cortex-agent-service-type
  (service-type
   (name 'cortex-agent)
   (description "Run the Palo Alto Networks Cortex XDR Agent daemon for
endpoint security, including Anti-Virus/Anti-Malware, EDR, Ransomware
Protection, and Exploit Prevention.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             cortex-agent-shepherd-service)
          (service-extension activation-service-type
                             cortex-agent-activation)
          (service-extension account-service-type
                             cortex-agent-accounts)
          (service-extension profile-service-type
                             (lambda (config)
                               (list (cortex-agent-configuration-package
                                      config))))))
   (default-value (cortex-agent-configuration))))
