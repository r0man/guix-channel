;; Example Guix System configuration with Cortex XDR Agent
;;
;; This is an example of how to configure the Cortex XDR Agent
;; service in your Guix System configuration.

(use-modules (gnu)
             (r0man guix services cortex))

(use-service-modules networking ssh)
(use-package-modules admin)

(operating-system
  ;; ... your other system configuration ...

  (services
   (cons*
    ;; Example 1: Basic Cortex XDR Agent service with default configuration
    (service cortex-agent-service-type)

    ;; Example 2: Configuration with distribution ID and server
    ;; (service cortex-agent-service-type
    ;;          (cortex-agent-configuration
    ;;           (distribution-id "your-distribution-id")
    ;;           (distribution-server "https://your-server.example.com")))

    ;; Example 3: Full configuration with all options
    ;; (service cortex-agent-service-type
    ;;          (cortex-agent-configuration
    ;;           ;; Distribution and server settings
    ;;           (distribution-id "prod-dist-001")
    ;;           (distribution-server "https://cortex.example.com")
    ;;
    ;;           ;; Endpoint tagging for organization
    ;;           (endpoint-tags '("production" "web-server" "zone-dmz"))
    ;;
    ;;           ;; Proxy configuration
    ;;           (proxy-list '("http://proxy1.example.com:8080"
    ;;                        "http://proxy2.example.com:8080"))
    ;;
    ;;           ;; User and kernel module settings
    ;;           (unprivileged-user "cortexuser")  ; default
    ;;           (kernel-module? #t)  ; enable kernel module (default)
    ;;
    ;;           ;; Session type (for VMs or temporary workloads)
    ;;           (temporary-session? #f)  ; not a temporary session
    ;;           (vm-template? #f)  ; not a VM template
    ;;
    ;;           ;; Security restrictions
    ;;           (restrictions '("live_terminal"
    ;;                          "script_execution"
    ;;                          "file_retrieval"))
    ;;           ;; Available restrictions:
    ;;           ;;   - "live_terminal": Disable Live Terminal feature
    ;;           ;;   - "script_execution": Disable script execution
    ;;           ;;   - "file_retrieval": Disable file retrieval
    ;;           ;;   - "all": Apply all restrictions
    ;;
    ;;           ;; Additional installer options
    ;;           (extra-options '("--library-compatibility"))))

    ;; Example 4: Using a custom cortex.conf file
    ;; (service cortex-agent-service-type
    ;;          (cortex-agent-configuration
    ;;           (config-file (local-file "/path/to/cortex.conf"))))

    %base-services)))

;; Usage notes:
;;
;; 1. Configuration file locations:
;;    - Generated config: /etc/panw/cortex.conf
;;    - Agent configs: /opt/traps/config/*.xml
;;    - Logs: /var/log/traps/pmd.log
;;
;; 2. Service management:
;;    # herd status cortex-agent
;;    # herd stop cortex-agent
;;    # herd start cortex-agent
;;    # herd restart cortex-agent
;;
;; 3. The cytool utility:
;;    # /run/current-system/profile/opt/traps/bin/cytool --help
;;    # /run/current-system/profile/opt/traps/bin/cytool status
;;
;; 4. Configuration generation:
;;    The service automatically generates /etc/panw/cortex.conf from
;;    the configuration fields. You can also provide a custom config-file.
;;
;; 5. Permissions:
;;    The cortex.conf file is automatically set to root:root with
;;    permissions 0600 for security.
;;
;; 6. Kernel module:
;;    Set (kernel-module? #f) to install without the kernel module.
;;    This disables behavioral threat protection but may be needed
;;    on systems with incompatible kernels.
;;
;; 7. Required directories:
;;    The service automatically creates all required directories:
;;    - /etc/panw (configuration)
;;    - /etc/traps (OS persistent data)
;;    - /var/log/traps (logs)
;;    - /opt/traps/* (runtime directories)
