;; Guix System configuration for testing Cortex XDR Agent in a VM
;;
;; This configuration creates a minimal system with the Cortex XDR Agent
;; service for testing purposes.

(use-modules (gnu)
             (gnu system)
             (gnu system vm)
             (r0man guix services cortex)
             (r0man guix packages cortex))

(use-service-modules base desktop networking ssh)
(use-package-modules admin linux lsof ssh)

(operating-system
  (host-name "cortex-xdr-test")
  (timezone "UTC")
  (locale "en_US.utf8")

  ;; Boot with QEMU firmware
  (firmware '())

  ;; Enable serial console output for headless VM testing
  (kernel-arguments '("console=ttyS0,115200"))

  ;; Use a simple bootloader for VMs
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/vda"))
               (terminal-outputs '(console))))

  ;; Simple partition scheme for VM
  (file-systems (cons (file-system
                        (device (file-system-label "root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  ;; Root account with password "root" for testing
  (users (cons (user-account
                 (name "test")
                 (comment "Test User")
                 (group "users")
                 (supplementary-groups '("wheel" "netdev"
                                        "audio" "video"))
                 (home-directory "/home/test"))
               %base-user-accounts))

  ;; Allow SSH with password for testing
  (sudoers-file
   (plain-file "sudoers"
               (string-append (plain-file-content %sudoers-specification)
                             "\n%wheel ALL=(ALL) NOPASSWD: ALL\n")))

  ;; System packages for testing and debugging
  ;; Note: nss-certs is included in %base-packages by default
  (packages (append (list
                     ;; Basic utilities
                     openssh

                     ;; Debugging tools
                     strace
                     lsof
                     htop

                     ;; Cortex XDR package (for cytool access)
                     cortex-agent)
                    %base-packages))

  (services
   (cons*
    ;; Cortex XDR Agent service with test configuration
    ;; NOTE: This is a basic configuration for VM testing.
    ;; In production, you would provide actual distribution-id
    ;; and distribution-server values.
    ;;
    ;; IMPORTANT: The agent requires nscd for NSS user lookups (IPC).
    ;; The %base-services below includes nscd-service-type by default.
    (service cortex-agent-service-type
             (cortex-agent-configuration
              ;; Test configuration - replace with real values
              (distribution-id "")
              (distribution-server "")
              ;; Tag this as a test endpoint
              (endpoint-tags '("test" "vm" "guix-system"))
              ;; Disable kernel module for VM testing if needed
              ;; (kernel-module? #f)
              ;; Test user
              (unprivileged-user "cortexuser")))

    ;; OpenSSH for remote access
    ;; WARNING: This config is INSECURE - for local VM testing only!
    ;; Never use in production: allows root login and empty passwords.
    (service openssh-service-type
             (openssh-configuration
              (permit-root-login #t)
              (password-authentication? #t)
              (allow-empty-passwords? #t)))

    ;; DHCP client for network
    (service dhcpcd-service-type)

    ;; Serial console for headless VM testing
    (service mingetty-service-type
             (mingetty-configuration
              (tty "ttyS0")
              (auto-login "root")))

    %base-services)))
