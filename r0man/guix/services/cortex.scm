;;; GNU Guix --- Functional package management for GNU
;;; Copyright (C) 2024 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of guix-channel-cortex.
;;;
;;; guix-channel-cortex is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; guix-channel-cortex is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guix-channel-cortex.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (r0man guix services cortex)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (r0man guix packages cortex)
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
;;;
;;; This module provides a Guix System service for running the Palo Alto
;;; Networks Cortex XDR endpoint security agent.  The service provides:
;;; - Anti-Virus/Anti-Malware protection
;;; - EDR (Endpoint Detection and Response)
;;; - Ransomware Protection
;;; - Exploit Prevention
;;; - Behavioral Threat Protection (requires kernel module)
;;;
;;; Quick Start:
;;;
;;; Add to your system configuration:
;;;
;;;   (use-modules (r0man guix services cortex))
;;;
;;;   (operating-system
;;;     ;; ...
;;;     (services
;;;      (cons* (service cortex-agent-service-type
;;;                      (cortex-agent-configuration
;;;                       (distribution-id "your-distribution-id")
;;;                       (distribution-server "https://your-server.com")))
;;;             %base-services)))
;;;
;;; Then reconfigure: sudo guix system reconfigure /etc/config.scm
;;;
;;; Configuration Options:
;;;
;;; The `cortex-agent-configuration` record supports these options:
;;;
;;;   package             - Cortex XDR Agent package (default: cortex-agent)
;;;   config-file         - Custom cortex.conf file (default: #f, auto-generated)
;;;   distribution-id     - Distribution ID from Cortex XDR console (required)
;;;   distribution-server - Cortex XDR management server URL (required)
;;;   endpoint-tags       - List of tags for endpoint organization
;;;   proxy-list          - List of HTTP proxy servers
;;;   unprivileged-user   - User for unprivileged operations (default: "cortexuser")
;;;   kernel-module?      - Enable kernel module (default: #t)
;;;   temporary-session?  - Temporary session mode for VDI (default: #f)
;;;   vm-template?        - VM template mode (default: #f)
;;;   restrictions        - List of feature restrictions
;;;   extra-options       - Additional installer options
;;;
;;; Available Restrictions:
;;;
;;;   "live_terminal"    - Disable Live Terminal feature
;;;   "script_execution" - Disable remote script execution
;;;   "file_retrieval"   - Disable file retrieval capability
;;;   "all"              - Apply all restrictions
;;;
;;; Service Management:
;;;
;;;   # Check status
;;;   sudo herd status cortex-agent
;;;
;;;   # Start/stop/restart
;;;   sudo herd start cortex-agent
;;;   sudo herd stop cortex-agent
;;;   sudo herd restart cortex-agent
;;;
;;;   # View logs
;;;   sudo tail -f /var/log/traps/pmd.log
;;;
;;;   # Use cytool utility
;;;   sudo /run/current-system/profile/opt/traps/bin/cytool status
;;;
;;; File Locations:
;;;
;;;   /etc/panw/cortex.conf      - Generated configuration file
;;;   /var/log/traps/pmd.log     - Agent log file
;;;   /opt/traps/                - Agent installation directory
;;;   /opt/traps/config/         - Agent XML configuration
;;;   /opt/traps/bin/            - Agent binaries (pmd, cytool, initd)
;;;
;;; Security Considerations:
;;;
;;; 1. Credentials: Store distribution-id and distribution-server securely.
;;;    Consider using Guix secrets management for production deployments.
;;;
;;; 2. Config Permissions: The /etc/panw/cortex.conf file is created with
;;;    mode 0600 (root-only readable) automatically.
;;;
;;; 3. Unprivileged User: The agent creates a dedicated cortexuser account
;;;    for unprivileged operations, minimizing root exposure.
;;;
;;; 4. Feature Restrictions: Use the restrictions option to limit agent
;;;    capabilities based on your security policy.
;;;
;;; 5. Network Access: The agent requires outbound HTTPS access to your
;;;    Cortex XDR server.  Configure firewalls accordingly.
;;;
;;; Known Limitations:
;;;
;;; 1. Kernel Module Compatibility
;;;    The kernel module requires a compatible kernel version (2.6.32+,
;;;    3.4+ recommended).  If you experience issues, disable it:
;;;      (kernel-module? #f)
;;;    This disables behavioral threat protection but allows the agent to run.
;;;
;;; 2. nscd Dependency
;;;    This service requires nscd (Name Service Cache Daemon) for user lookups
;;;    needed by IPC operations.  The nscd service is included in %base-services,
;;;    so typical system configurations will have it.  Without nscd, user lookup
;;;    operations (getpwnam, etc.) will fail because the patched binaries use
;;;    Guix's glibc but cannot load the system's NSS modules directly.
;;;
;;;    If using a minimal service configuration, ensure nscd is included:
;;;      (services
;;;       (cons* (service nscd-service-type)
;;;              (service cortex-agent-service-type)
;;;              ;; ...
;;;              ))
;;;
;;; Troubleshooting:
;;;
;;; Service Won't Start:
;;;   Check logs: sudo tail -100 /var/log/traps/pmd.log
;;;   Common causes:
;;;   - Missing or invalid distribution-id/distribution-server
;;;   - Network connectivity issues to Cortex XDR server
;;;   - License validation failures
;;;   - Kernel module compatibility issues
;;;
;;; Binary Execution Errors:
;;;   Check library paths: ldd /run/current-system/profile/opt/traps/bin/pmd
;;;   Verify interpreter: file /run/current-system/profile/opt/traps/bin/pmd
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

        ;; Symlink common utilities to FHS locations (/bin and /usr/bin)
        (let ((profile "/run/current-system/profile")
              (utils '("env" "sh" "cat" "grep" "sed" "awk" "id" "whoami"
                       "uname" "hostname" "date" "ls" "cp" "mv" "rm" "mkdir"
                       "chmod" "chown" "ps" "kill" "sleep" "head" "tail" "wc"
                       "sort" "uniq" "cut" "tr" "tee" "xargs" "find" "which"
                       "stat" "readlink" "dirname" "basename")))
          (for-each
           (lambda (cmd)
             (let ((src (string-append profile "/bin/" cmd)))
               (when (file-exists? src)
                 ;; Symlink to /usr/bin
                 (let ((dst (string-append "/usr/bin/" cmd)))
                   (unless (file-exists? dst)
                     (symlink src dst)))
                 ;; Symlink to /bin for scripts that use /bin/grep, etc.
                 (let ((dst (string-append "/bin/" cmd)))
                   (unless (file-exists? dst)
                     (symlink src dst))))))
           utils))

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
                     ;; Note: Do NOT include /opt/traps/glibc/lib here.
                     ;; The binaries are patched to use Guix's glibc, which
                     ;; enables NSS lookups via nscd. Including the bundled
                     ;; glibc in LD_LIBRARY_PATH would override this and
                     ;; break user lookups needed for IPC.
                     (list "LD_LIBRARY_PATH=/opt/traps/lib"
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

;;; cortex.scm ends here
