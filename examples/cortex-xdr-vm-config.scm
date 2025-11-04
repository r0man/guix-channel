;; Guix System configuration for testing Cortex XDR Agent in a VM
;;
;; This configuration creates a minimal system with the Cortex XDR Agent
;; service for testing purposes.

(use-modules (gnu)
             (gnu system)
             (gnu system vm)
             (r0man guix services security)
             (r0man guix packages security))

(use-service-modules desktop networking ssh)
(use-package-modules admin certs ssh)

(operating-system
  (host-name "cortex-xdr-test")
  (timezone "UTC")
  (locale "en_US.utf8")

  ;; Boot with QEMU firmware
  (firmware '())

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
  (packages (append (list
                     ;; Basic utilities
                     openssh
                     nss-certs

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
    (service openssh-service-type
             (openssh-configuration
              (permit-root-login #t)
              (password-authentication? #t)))

    ;; DHCP client for network
    (service dhcpcd-service-type)

    %base-services)))
