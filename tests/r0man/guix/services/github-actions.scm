;;; Unit tests for the multi-instance support of the github-actions-runner
;;; service types: expansion of the `instances' field, the per-instance
;;; Shepherd service names, work directories and log files, the checks on
;;; the service value, and the accounts shared by instances.  Nothing is
;;; built or booted; the VM test in github-actions-system.scm covers the
;;; running system.

(define-module (test-r0man-guix-services-github-actions)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (guix diagnostics)
  #:use-module (r0man guix services github-actions)
  #:use-module (r0man guix home services github-actions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

;; The extension procedures are private; reach them directly.
(define runner-accounts
  (@@ (r0man guix services github-actions) github-actions-runner-accounts))
(define runner-shepherd-services
  (@@ (r0man guix services github-actions)
      github-actions-runner-shepherd-services))
(define home-runner-shepherd-services
  (@@ (r0man guix home services github-actions)
      home-github-actions-runner-shepherd-services))

(define (error-message thunk)
  "Call THUNK and return the text of the formatted message it raises, or
#f if it returns normally."
  (guard (c ((formatted-message? c)
             (apply format #f (formatted-message-string c)
                    (formatted-message-arguments c))))
    (thunk)
    #f))

(define (runner . fields)
  ;; Shorthand for a configuration pointed at a repository.
  (apply (lambda* (#:key (id #f) (instances 1) (name #f) (work-dir #f)
                   (log-file #f) (user "github-actions-runner")
                   (group "github-actions-runner")
                   (supplementary-groups '()))
           (github-actions-runner-configuration
            (id id) (instances instances) (name name)
            (work-dir work-dir) (log-file log-file)
            (user user) (group group)
            (supplementary-groups supplementary-groups)
            (url "https://github.com/example/example")))
         fields))

(test-begin "github-actions-runner-instances")

(test-equal "the default value is one instance without id"
  '(github-actions-runner)
  (map github-actions-runner-provision
       (github-actions-runner-configurations
        (service-type-default-value github-actions-runner-service-type))))

(test-equal "the home default value is one instance without id"
  '(github-actions-runner)
  (map github-actions-runner-provision
       (github-actions-runner-configurations
        (service-type-default-value
         home-github-actions-runner-service-type))))

(test-equal "instances expand to suffixed ids"
  '("bs-1" "bs-2" "bs-3")
  (map github-actions-runner-configuration-id
       (github-actions-runner-configurations
        (list (runner #:id "bs" #:instances 3)))))

(test-equal "instances without id expand to numeric ids"
  '("1" "2")
  (map github-actions-runner-configuration-id
       (github-actions-runner-configurations
        (list (runner #:instances 2)))))

(test-equal "instances suffix an explicit name and leave a missing one"
  '("m1-1" "m1-2" #f #f)
  (map github-actions-runner-configuration-name
       (github-actions-runner-configurations
        (list (runner #:id "a" #:instances 2 #:name "m1")
              (runner #:id "b" #:instances 2)))))

(test-equal "expanded instances are single instances"
  '(1 1)
  (map github-actions-runner-configuration-instances
       (github-actions-runner-configurations
        (list (runner #:id "a" #:instances 2)))))

(test-equal "provisions derive from the id"
  '(github-actions-runner github-actions-runner-a github-actions-runner-bs-1)
  (map github-actions-runner-provision
       (list (runner) (runner #:id "a") (runner #:id "bs-1"))))

(test-equal "work directories derive from the id unless explicit"
  (list %github-actions-runner-state-directory
        (string-append %github-actions-runner-state-directory "/a")
        "/scratch/runner")
  (map github-actions-runner-work-dir
       (list (runner) (runner #:id "a")
             (runner #:id "b" #:work-dir "/scratch/runner"))))

(test-equal "log files derive from the id unless explicit"
  '("/var/log/github-actions-runner.log"
    "/var/log/github-actions-runner-a.log"
    "/dev/console")
  (map github-actions-runner-log-file
       (list (runner) (runner #:id "a")
             (runner #:id "b" #:log-file "/dev/console"))))

(test-equal "runner config files follow the expanded work directories"
  (list (string-append %github-actions-runner-state-directory "/bs-1/.runner")
        (string-append %github-actions-runner-state-directory "/bs-2/.runner")
        "/scratch/runner/.runner")
  (github-actions-runner-config-files
   (list (runner #:id "bs" #:instances 2)
         (runner #:id "c" #:work-dir "/scratch/runner"))))

(test-assert "a bare configuration is rejected"
  (string-contains
   (error-message
    (lambda () (github-actions-runner-configurations (runner))))
   "must be a list"))

(test-assert "two instances without id are rejected"
  (string-contains
   (error-message
    (lambda ()
      (github-actions-runner-configurations (list (runner) (runner)))))
   "'github-actions-runner' provided more than once"))

(test-assert "an expanded id clashing with an explicit one is rejected"
  (string-contains
   (error-message
    (lambda ()
      (github-actions-runner-configurations
       (list (runner #:id "a" #:instances 2) (runner #:id "a-2")))))
   "'github-actions-runner-a-2' provided more than once"))

(test-assert "an id with a slash is rejected"
  (string-contains
   (error-message
    (lambda ()
      (github-actions-runner-configurations (list (runner #:id "a/b")))))
   "invalid instance id"))

(test-assert "an empty id is rejected"
  (string-contains
   (error-message
    (lambda ()
      (github-actions-runner-configurations (list (runner #:id "")))))
   "invalid instance id"))

(test-assert "instances must be a positive integer"
  (string-contains
   (error-message
    (lambda ()
      (github-actions-runner-configurations (list (runner #:instances 0)))))
   "positive integer"))

(test-assert "an explicit work directory cannot be shared by instances"
  (string-contains
   (error-message
    (lambda ()
      (github-actions-runner-configurations
       (list (runner #:id "a" #:instances 2 #:work-dir "/scratch/runner")))))
   "'work-dir' cannot be set"))

(test-assert "an explicit log file cannot be shared by instances"
  (string-contains
   (error-message
    (lambda ()
      (github-actions-runner-configurations
       (list (runner #:id "a" #:instances 2 #:log-file "/dev/console")))))
   "'log-file' cannot be set"))

(test-end "github-actions-runner-instances")


(test-begin "github-actions-runner-system-service")

(test-equal "one shepherd service per instance"
  '((github-actions-runner-bs-1) (github-actions-runner-bs-2)
    (github-actions-runner-c))
  (map shepherd-service-provision
       (runner-shepherd-services
        (list (runner #:id "bs" #:instances 2) (runner #:id "c")))))

(test-equal "instances sharing an account get one account and one group"
  '("github-actions-runner")
  (let ((accounts (runner-accounts
                   (list (runner #:id "a") (runner #:id "b")))))
    (map user-account-name (filter user-account? accounts))))

(test-equal "the shared account joins the groups of all its instances"
  '("docker" "kvm")
  (let* ((accounts (runner-accounts
                    (list (runner #:id "a" #:supplementary-groups '("docker"))
                          (runner #:id "b" #:supplementary-groups
                                  '("kvm" "docker")))))
         (account (find user-account? accounts)))
    (sort (user-account-supplementary-groups account) string<?)))

(test-equal "every group is created once"
  '("docker" "github-actions-runner" "kvm")
  (let ((accounts (runner-accounts
                   (list (runner #:id "a" #:supplementary-groups '("docker"))
                         (runner #:id "b" #:supplementary-groups
                                 '("kvm" "docker"))))))
    (sort (map user-group-name (filter user-group? accounts)) string<?)))

(test-equal "the shared account's home is the state directory"
  %github-actions-runner-state-directory
  (user-account-home-directory
   (find user-account?
         (runner-accounts (list (runner #:id "a") (runner #:id "b"))))))

(test-equal "an explicit work directory becomes the account's home"
  "/scratch/runner"
  (user-account-home-directory
   (find user-account?
         (runner-accounts (list (runner #:work-dir "/scratch/runner"))))))

(test-equal "distinct users get distinct accounts"
  '("runner-a" "runner-b")
  (sort (map user-account-name
             (filter user-account?
                     (runner-accounts
                      (list (runner #:id "a" #:user "runner-a" #:group "ra")
                            (runner #:id "b" #:user "runner-b" #:group "rb")))))
        string<?))

(test-equal "no account is created for root"
  '()
  (filter user-account?
          (runner-accounts (list (runner #:user "root" #:group "root")))))

(test-end "github-actions-runner-system-service")


(test-begin "github-actions-runner-home-service")

(test-equal "one home shepherd service per instance"
  '((github-actions-runner) (github-actions-runner-a))
  (map shepherd-service-provision
       (home-runner-shepherd-services
        (list (runner) (runner #:id "a")))))

(test-equal "home log files live under XDG_STATE_HOME and derive from the id"
  '("/state/github-actions-runner.log"
    "/state/github-actions-runner-a.log"
    "/tmp/runner.log")
  (let ((state-home (getenv "XDG_STATE_HOME")))
    (dynamic-wind
      (lambda () (setenv "XDG_STATE_HOME" "/state"))
      (lambda ()
        (map home-github-actions-runner-log-file
             (list (runner) (runner #:id "a")
                   (runner #:id "b" #:log-file "/tmp/runner.log"))))
      (lambda ()
        (if state-home
            (setenv "XDG_STATE_HOME" state-home)
            (unsetenv "XDG_STATE_HOME"))))))

(test-end "github-actions-runner-home-service")
