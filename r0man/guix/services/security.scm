(define-module (r0man guix services security)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (r0man guix packages security)
  #:export (cortex-agent-configuration
            cortex-agent-configuration?
            cortex-agent-service-type))

;;; Commentary:
;;;
;;; Cortex XDR Agent service for Guix System.
;;; Provides endpoint security including Anti-Virus, EDR, Ransomware Protection,
;;; and Exploit Prevention.
;;;
;;; Code:

(define-record-type* <cortex-agent-configuration>
  cortex-agent-configuration make-cortex-agent-configuration
  cortex-agent-configuration?
  (package          cortex-agent-configuration-package
                    (default cortex-agent))
  (config-file      cortex-agent-configuration-config-file
                    (default #f))
  (distribution-id  cortex-agent-configuration-distribution-id
                    (default ""))
  (distribution-server cortex-agent-configuration-distribution-server
                       (default ""))
  (extra-options    cortex-agent-configuration-extra-options
                    (default '())))

(define (cortex-agent-activation config)
  "Return the activation gexp for Cortex XDR Agent."
  #~(begin
      (use-modules (guix build utils))
      ;; Create necessary directories
      (mkdir-p "/etc/panw")
      (mkdir-p "/var/log/traps")
      (mkdir-p "/opt/traps/persist")
      (mkdir-p "/opt/traps/download")
      (mkdir-p "/opt/traps/tmp")

      ;; Copy config file if provided
      #$@(if (cortex-agent-configuration-config-file config)
             #~((copy-file #$(cortex-agent-configuration-config-file config)
                          "/etc/panw/cortex.conf"))
             #~())))

(define (cortex-agent-shepherd-service config)
  "Return a Shepherd service for Cortex XDR Agent."
  (let ((package (cortex-agent-configuration-package config)))
    (list (shepherd-service
           (documentation "Palo Alto Networks Cortex XDR Agent daemon.")
           (provision '(cortex-agent traps-pmd))
           (requirement '(networking file-systems))
           (start #~(make-forkexec-constructor
                     (list (string-append #$package "/opt/traps/bin/pmd"))
                     #:log-file "/var/log/traps/pmd.log"
                     #:environment-variables
                     (list (string-append "LD_LIBRARY_PATH="
                                         #$package "/opt/traps/lib:"
                                         #$package "/opt/traps/glibc/lib")
                           "PATH=/run/current-system/profile/bin")))
           (stop #~(lambda (pid)
                     ;; First stop the pmd process
                     (kill pid SIGTERM)
                     (sleep 2)
                     ;; Run km_manage stop if it exists
                     (let ((km-manage (string-append #$package
                                                     "/opt/traps/km_utils/km_manage")))
                       (when (file-exists? km-manage)
                         (system* km-manage "stop")))
                     #f))
           (respawn? #t)
           (actions
            (list (shepherd-action
                   (name 'status)
                   (documentation "Check the status of the Cortex XDR agent.")
                   (procedure
                    #~(lambda (pid)
                        (if pid
                            (format #t "Cortex XDR agent is running (PID ~a).~%"
                                   pid)
                            (format #t "Cortex XDR agent is not running.~%")))))))))))

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
          (service-extension profile-service-type
                             (lambda (config)
                               (list (cortex-agent-configuration-package
                                      config))))))
   (default-value (cortex-agent-configuration))))
