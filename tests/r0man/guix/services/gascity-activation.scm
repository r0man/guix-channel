;;; Unit tests for the root half of the Gas City system activation program
;;; in (r0man guix services gascity): root only creates the state and log
;;; directories, never follows a symbolic link the service user planted, and
;;; writes everything below the state directory as the service user.
;;;
;;; The program must run as root to chown, so each scenario runs it inside an
;;; unprivileged user and mount namespace (`unshare --map-auto
;;; --map-root-user --mount'), on a fresh tmpfs mounted at /mnt, with
;;; `nobody:nogroup' as the service account.  The program text comes from
;;; `gexp->approximate-sexp', like the other gascity tests, so no store or
;;; daemon is needed.  When the host cannot create such a namespace (no
;;; subordinate id range, user namespaces disabled) the scenarios are
;;; skipped.

(define-module (test-r0man-guix-services-gascity-activation)
  #:use-module (guix gexp)
  #:use-module (r0man guix services gascity)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports))

(define %config
  (gascity-supervisor-configuration
   (user "nobody")
   (group "nogroup")
   (state-directory "/mnt/gascity/state")
   (log-directory "/mnt/gascity/log")))

(define %program
  ;; The activation program, as a file that plain `guile' can run.
  (let ((file (string-append (or (getenv "TMPDIR") "/tmp")
                             "/gascity-activation-test-"
                             (number->string (getpid)) ".scm")))
    (call-with-output-file file
      (lambda (port)
        (write (gexp->approximate-sexp
                (program-file-gexp
                 (gascity-supervisor-activation-program %config)))
               port)))
    file))

(define %guix-modules
  ;; The directory holding (guix build utils), the program's only import.
  (let ((utils (search-path %load-path "guix/build/utils.scm")))
    (dirname (dirname (dirname utils)))))

(define (run-scenario setup report)
  "Run the activation program in a fresh namespace after the shell commands
SETUP, then the shell commands REPORT.  Return the combined output, which
starts with a `status=N' line for the program's exit status."
  (let* ((script
          (string-append
           "set -e\n"
           "mount -t tmpfs -o mode=755 none /mnt\n"
           "mkdir -m 755 /mnt/target\n"
           "echo secret > /mnt/target/file\n"
           "chmod 600 /mnt/target/file\n"
           setup "\n"
           "set +e\n"
           "guile --no-auto-compile -L " %guix-modules " " %program
           " >/dev/null 2>&1\n"
           "echo status=$?\n"
           report "\n"))
         (port (open-pipe* OPEN_READ "unshare" "--map-auto" "--map-root-user"
                           "--mount" "sh" "-c" script))
         (output (get-string-all port)))
    (close-pipe port)
    output))

(define (stat-line file)
  "Return the shell command that prints owner, group, mode and type of FILE."
  (string-append "stat -c '%n %U %G %a %F' " file))

(define %namespaces?
  ;; Whether this host can run the scenarios at all.
  (string-contains (run-scenario "" "echo ok") "ok"))

(test-begin "gascity-activation")

(unless %namespaces?
  (test-skip 100))

(test-group "a fresh activation"
  (define output
    (run-scenario
     ""
     (string-join
      (map stat-line
           '("/mnt/gascity/state" "/mnt/gascity/log"
             "/mnt/gascity/log/supervisor-provision.log"
             "/mnt/gascity/state/.gc/supervisor.toml"
             "/mnt/gascity/state/.dolt/config_global.json"))
      "\n")))

  (test-assert "it succeeds"
    (string-contains output "status=0"))
  (test-assert "the state directory belongs to the service user"
    (string-contains output "/mnt/gascity/state nobody nogroup 700 directory"))
  (test-assert "the log directory stays root's, readable by the group"
    (string-contains output "/mnt/gascity/log root nogroup 750 directory"))
  (test-assert "root pre-creates the provision log for the service user"
    (string-contains output
                     "supervisor-provision.log nobody nogroup 640 regular"))
  (test-assert "files below the state directory are written as the user"
    (and (string-contains output
                          "supervisor.toml nobody nogroup 600 regular")
         (string-contains output
                          "config_global.json nobody nogroup 600 regular"))))

(test-group "a symbolic link planted below the state directory"
  (define output
    (run-scenario
     (string-append "mkdir -p /mnt/gascity/state\n"
                    "chown 65534:997 /mnt/gascity/state\n"
                    "ln -s /mnt/target /mnt/gascity/state/.gc\n"
                    "chown -h 65534:997 /mnt/gascity/state/.gc")
     (string-append (stat-line "/mnt/target") "\n"
                    "ls /mnt/target")))

  (test-assert "the target directory keeps its owner and mode"
    (string-contains output "/mnt/target root root 755 directory"))
  (test-assert "nothing is written into the target"
    (not (string-contains output "supervisor.toml"))))

(test-group "a state directory that is a symbolic link"
  (define output
    (run-scenario
     (string-append "mkdir -p /mnt/gascity\n"
                    "ln -s /mnt/target /mnt/gascity/state")
     (stat-line "/mnt/target")))

  (test-assert "activation refuses it"
    (string-contains output "status=1"))
  (test-assert "the target keeps its owner and mode"
    (string-contains output "/mnt/target root root 755 directory")))

(test-group "symbolic links planted in a formerly user-owned log directory"
  (define output
    (run-scenario
     (string-append "mkdir -p /mnt/gascity/log\n"
                    "chown 65534:997 /mnt/gascity/log\n"
                    "chmod 700 /mnt/gascity/log\n"
                    "ln -s /mnt/target/file /mnt/gascity/log/supervisor.log\n"
                    "ln -s /mnt/target/file "
                    "/mnt/gascity/log/supervisor-provision.log")
     (string-join
      (list (stat-line "/mnt/target/file")
            "cat /mnt/target/file"
            (stat-line "/mnt/gascity/log")
            (stat-line "/mnt/gascity/log/supervisor-provision.log")
            "test -e /mnt/gascity/log/supervisor.log || echo log-removed")
      "\n")))

  (test-assert "it succeeds"
    (string-contains output "status=0"))
  (test-assert "the target file is untouched"
    (and (string-contains output "/mnt/target/file root root 600 regular")
         (string-contains output "secret")))
  (test-assert "the log directory is taken back by root"
    (string-contains output "/mnt/gascity/log root nogroup 750 directory"))
  (test-assert "the planted supervisor log link is removed"
    (string-contains output "log-removed"))
  (test-assert "the provision log is a fresh regular file"
    (string-contains output
                     "supervisor-provision.log nobody nogroup 640 regular")))

(test-group "a state directory below a user-owned parent"
  (define output
    (run-scenario
     (string-append "mkdir -p /mnt/gascity\n"
                    "chown 65534:997 /mnt/gascity")
     "test -e /mnt/gascity/state || echo state-absent"))

  (test-assert "activation refuses to manage it as root"
    (and (string-contains output "status=1")
         (string-contains output "state-absent"))))

(test-end "gascity-activation")

(delete-file %program)
