;; Example Guix System configuration with Cortex XDR Agent
;;
;; This is an example of how to configure the Cortex XDR Agent
;; service in your Guix System configuration.

(use-modules (gnu)
             (r0man guix services security))

(use-service-modules networking ssh)
(use-package-modules admin)

(operating-system
  ;; ... your other system configuration ...

  (services
   (cons*
    ;; Basic Cortex XDR Agent service with default configuration
    (service cortex-agent-service-type)

    ;; Or with custom configuration:
    ;; (service cortex-agent-service-type
    ;;          (cortex-agent-configuration
    ;;           (config-file (local-file "/path/to/cortex.conf"))
    ;;           (distribution-id "your-distribution-id")
    ;;           (distribution-server "your-server-url")))

    %base-services)))

;; Usage notes:
;;
;; 1. The config file should be placed in /etc/panw/cortex.conf
;;    You can provide it via the config-file field or manually.
;;
;; 2. To check service status:
;;    # herd status cortex-agent
;;
;; 3. To view logs:
;;    # tail -f /var/log/traps/pmd.log
;;
;; 4. To manually control the service:
;;    # herd stop cortex-agent
;;    # herd start cortex-agent
;;    # herd restart cortex-agent
;;
;; 5. The cytool utility is available at:
;;    /run/current-system/profile/opt/traps/bin/cytool
