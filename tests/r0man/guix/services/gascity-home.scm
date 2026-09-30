;;; Unit tests for the Home Gas City service: the home path defaults, the
;;; validation of the service value, the suffixed Home Shepherd service names
;;; and their requirements, the activation that creates the state directory,
;;; and the mapping of a system service value to the Home service type.
;;; Nothing is built or booted.

(define-module (test-r0man-guix-services-gascity-home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (r0man guix home services gascity)
  #:use-module (r0man guix services gascity)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

;; The extension procedures are private; reach them directly.
(define home-shepherd-services
  (@@ (r0man guix home services gascity)
      home-gascity-supervisor-shepherd-services))
(define home-activation
  (@@ (r0man guix home services gascity)
      home-gascity-supervisor-activation))

(define (supervisor . fields)
  ;; Shorthand for a supervisor configuration.  A single instance does not
  ;; have to set a port, so default to the one Gas City uses.
  (apply (lambda* (#:key (id #f) (port 8372) (state-directory 'unset)
                   (cities '()))
           (gascity-supervisor-configuration
            (id id) (port port) (state-directory state-directory)
            (cities cities)))
         fields))

(define (error-message thunk)
  "Call THUNK and return the text of the formatted message it raises, or #f
if it returns normally."
  (guard (c ((formatted-message? c)
             (apply format #f (formatted-message-string c)
                    (formatted-message-arguments c))))
    (thunk)
    #f))

(define (with-environment bindings thunk)
  "Call THUNK with BINDINGS, a list of (NAME . VALUE), in the environment.
A #f VALUE unsets NAME.  The previous environment is restored afterwards."
  (define names (map car bindings))
  (define saved (map (lambda (name) (cons name (getenv name))) names))
  (define (apply-bindings bindings)
    (for-each (lambda (binding)
                (if (cdr binding)
                    (setenv (car binding) (cdr binding))
                    (unsetenv (car binding))))
              bindings))
  (dynamic-wind
    (lambda () (apply-bindings bindings))
    thunk
    (lambda () (apply-bindings saved))))

(define (service-named name services)
  (find (lambda (service)
          (memq name (shepherd-service-provision service)))
        services))

(define (approximate-string thing)
  (format #f "~s" (gexp->approximate-sexp thing)))


(test-begin "gascity-home")

;;;
;;; Home path defaults.
;;;

(test-equal "the state directory follows XDG_STATE_HOME"
  "/state/gascity"
  (with-environment '(("XDG_STATE_HOME" . "/state"))
    home-gascity-state-directory))

(test-equal "the state directory falls back to HOME"
  "/home/alice/.local/state/gascity"
  (with-environment '(("XDG_STATE_HOME" . #f) ("HOME" . "/home/alice"))
    home-gascity-state-directory))

(test-equal "the log directory is the state directory"
  "/home/alice/.local/state/gascity"
  (with-environment '(("XDG_STATE_HOME" . #f) ("HOME" . "/home/alice"))
    home-gascity-log-directory))

(test-equal "gc-home follows HOME"
  "/home/alice/.gc"
  (with-environment '(("HOME" . "/home/alice"))
    home-gascity-gc-home))

(test-equal "home defaults fill the unset paths of a configuration"
  '("/home/alice/.local/state/gascity"
    "/home/alice/.local/state/gascity"
    "/home/alice/.gc")
  (with-environment '(("XDG_STATE_HOME" . #f) ("HOME" . "/home/alice"))
    (lambda ()
      (let ((config (home-gascity-supervisor-configuration
                     (gascity-supervisor-configuration))))
        (list (gascity-supervisor-state-directory config)
              (gascity-supervisor-log-directory config)
              (gascity-supervisor-gc-home config))))))

(test-equal "an explicit state directory is not overridden by the home default"
  "/scratch/state"
  (with-environment '(("XDG_STATE_HOME" . #f) ("HOME" . "/home/alice"))
    (lambda ()
      (gascity-supervisor-state-directory
       (home-gascity-supervisor-configuration
        (gascity-supervisor-configuration
         (state-directory "/scratch/state")))))))

(test-equal "an explicit gc-home is not overridden by the home default"
  "/scratch/gc"
  (with-environment '(("HOME" . "/home/alice"))
    (lambda ()
      (gascity-supervisor-gc-home
       (home-gascity-supervisor-configuration
        (gascity-supervisor-configuration
         (gc-home "/scratch/gc")))))))

(test-equal "city directories default under the home state directory"
  '("/home/alice/.local/state/gascity/swarm")
  (with-environment '(("XDG_STATE_HOME" . #f) ("HOME" . "/home/alice"))
    (lambda ()
      (let* ((config (home-gascity-supervisor-configuration
                      (gascity-supervisor-configuration
                       (cities (list (gascity-city-configuration
                                      (name "swarm")))))))
             (cities (gascity-supervisor-configuration-cities config)))
        (map (lambda (city) (gascity-city-directory config city))
             cities)))))

;;;
;;; The service value.
;;;

(test-equal "the default value is one instance whose paths are the home ones"
  (list (home-gascity-state-directory) (home-gascity-gc-home))
  (let ((configs (home-gascity-supervisor-configurations
                  (service-type-default-value home-gascity-service-type))))
    (list (gascity-supervisor-state-directory (car configs))
          (gascity-supervisor-gc-home (car configs)))))

(test-equal "a home value normalizes to its configurations"
  1
  (length (home-gascity-supervisor-configurations
           (list (supervisor)))))

(test-equal "the home value derives the state directory from XDG_STATE_HOME"
  (list "/state/gascity" "/state/gascity")
  (with-environment '(("XDG_STATE_HOME" . "/state"))
    (lambda ()
      (let ((config (car (home-gascity-supervisor-configurations
                          (list (supervisor))))))
        (list (gascity-supervisor-state-directory config)
              (gascity-supervisor-log-directory config))))))

(test-assert "a bare configuration is rejected"
  (string-contains
   (error-message (lambda () (home-gascity-supervisor-configurations #f)))
   "must be a list"))

(test-assert "two instances sharing a gc-home are rejected"
  (string-contains
   (error-message
    (lambda ()
      (home-gascity-supervisor-configurations
       (list (supervisor #:id "a" #:port 8372)
             (supervisor #:id "b" #:port 8373)))))
   "gc-home"))

;;;
;;; Home Shepherd services.
;;;

(test-equal "one home provision one-shot and supervisor per instance"
  '((home-gascity-provision) (home-gascity-supervisor))
  (map shepherd-service-provision
       (home-shepherd-services (list (supervisor)))))

(test-equal "the home provision and supervisor names are suffixed with the id"
  '((home-gascity-provision-a) (home-gascity-supervisor-a))
  (map shepherd-service-provision
       (home-shepherd-services (list (supervisor #:id "a")))))

(test-equal "the home supervisor requires the home provision one-shot"
  '(home-gascity-provision)
  (shepherd-service-requirement
   (service-named 'home-gascity-supervisor
                  (home-shepherd-services (list (supervisor))))))

(test-equal "the suffixed home supervisor requires its suffixed one-shot"
  '(home-gascity-provision-a)
  (shepherd-service-requirement
   (service-named 'home-gascity-supervisor-a
                  (home-shepherd-services (list (supervisor #:id "a"))))))

(test-assert "the home provision one-shot is a one-shot"
  (shepherd-service-one-shot?
   (service-named 'home-gascity-provision
                  (home-shepherd-services (list (supervisor))))))

(test-assert "the home supervisor start drops #:user"
  (let ((service (service-named 'home-gascity-supervisor
                                (home-shepherd-services (list (supervisor))))))
    (not (string-contains (approximate-string (shepherd-service-start service))
                          "#:user"))))

(test-assert "the home provision one-shot start drops #:user"
  (let ((service (service-named 'home-gascity-provision
                                (home-shepherd-services (list (supervisor))))))
    (not (string-contains (approximate-string (shepherd-service-start service))
                          "#:user"))))

(test-equal "the home supervisor log file is under the state directory"
  (string-append (home-gascity-log-directory) "/supervisor-a.log")
  (gascity-supervisor-log-file
   (car (home-gascity-supervisor-configurations
         (list (supervisor #:id "a"))))))

(test-assert "the home activation creates the state directory"
  (string-contains
   (approximate-string (home-activation (list (supervisor))))
   (home-gascity-state-directory)))

;;;
;;; System to Home service mapping.
;;;
;;; Guix does not map the services of a Home environment through
;;; 'system->home-service-type' (see (gnu home services)); the mapping is
;;; applied to the extension targets of a system service type, and
;;; 'system->home-service-type' is the documented mapping API.  The entry
;;; 'gascity-service-type => home-gascity-service-type' makes a system
;;; service type that extends the system Gas City service derive to the Home
;;; service type.

;; A stand-in for such a service type.
(define gascity-container-service-type
  (service-type
   (name 'gascity-container)
   (extensions (list (service-extension gascity-service-type (const '()))))
   (description "A stand-in for a system service type that extends Gas City.")))

(test-eq "a gascity extension is mapped to the home service type"
  home-gascity-service-type
  (service-extension-target
   (find (lambda (extension)
           (eq? (service-type-name (service-extension-target extension))
                'home-gascity))
         (service-type-extensions
          (system->home-service-type gascity-container-service-type)))))

(test-equal "the derived home type extends shepherd, activation and profile"
  (list home-shepherd-service-type
        home-activation-service-type
        home-profile-service-type)
  (map service-extension-target
       (service-type-extensions home-gascity-service-type)))

(test-end "gascity-home")
