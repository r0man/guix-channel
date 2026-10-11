;;; Unit tests for how the Gas City service keeps a city in step with `gc':
;;;
;;;   - the shared `gascity-sync-packs' and `gascity-soft-reload' procedures;
;;;   - the activation, which writes `.gc/site.toml' before `city.toml' (so the
;;;     reload the new `city.toml' triggers already sees the rig paths), and,
;;;     for a running city whose files it changed, re-syncs its imports and
;;;     asks its controller for a soft reload;
;;;   - the provision one-shot, which re-syncs the imports of an
;;;     `install-packs?' city on every run without ever failing for it.
;;;
;;; `gc' is a shell script that records its arguments and fails on demand, and
;;; the programs run as the current user (the Home variant), so neither a
;;; store nor a real `gc' is needed.

(define-module (test-r0man-guix-services-gascity-lifecycle)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (r0man guix services gascity)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports))

(define %root
  (let ((directory (string-append (or (getenv "TMPDIR") "/tmp")
                                  "/gascity-lifecycle-test-"
                                  (number->string (getpid)))))
    (when (file-exists? directory)
      (delete-file-recursively directory))
    (mkdir-p directory)
    directory))

(define (path . components)
  (string-join (cons %root components) "/"))

(define (read-file file)
  (if (file-exists? file)
      (call-with-input-file file get-string-all)
      ""))

(define (touch file)
  (call-with-output-file file (const #t)))

(define %gc-log (path "gc.log"))

(define %gc
  ;; A stand-in for `gc': it appends its arguments to `gc.log', fails `import
  ;; check' / `import install' / `reload' while `check-fails' /
  ;; `install-fails' / `reload-fails' exist, and makes `init' create the
  ;; runtime scaffold the provision program checks for.
  (let ((file (path "gc")))
    (call-with-output-file file
      (lambda (port)
        (format port "#!~a
echo \"$*\" >> ~a
case \"$1 $2\" in
  'import check') [ -e ~a ] && exit 1 ;;
  'import install') [ -e ~a ] && exit 1 ;;
  'reload --soft') [ -e ~a ] && exit 1 ;;
  'init --file')
    for city; do :; done
    ~a -p \"$city/.gc/runtime\"
    ~a \"$city/.gc/events.jsonl\" ;;
esac
exit 0
"
                (search-path (parse-path (getenv "PATH")) "sh")
                %gc-log (path "check-fails") (path "install-fails")
                (path "reload-fails")
                ;; The programs replace PATH.
                (which "mkdir") (which "touch"))))
    (chmod file #o755)
    file))

(define (gc-calls)
  "Return the `gc' invocations recorded since the last call, one string each."
  (let ((calls (filter (negate string-null?)
                       (string-split (read-file %gc-log) #\newline))))
    (when (file-exists? %gc-log)
      (delete-file %gc-log))
    calls))

(define %guix-modules
  ;; The directory holding (guix build utils).
  (let ((utils (search-path %load-path "guix/build/utils.scm")))
    (dirname (dirname (dirname utils)))))

(define (approximate thing)
  "Return the approximate S-expression of THING, a gexp, with nested gexps
approximated too, `gc' bound to the stand-in, and every other store item
replaced by a file name that does not exist."
  (cond ((gexp? thing) (approximate (gexp->approximate-sexp thing)))
        ((and (pair? thing)
              (eq? 'define (car thing))
              (pair? (cdr thing))
              (eq? 'gc (cadr thing)))
         `(define gc ,%gc))
        ((or (equal? thing '(*approximate*))
             (file-like? thing)
             (file-append? thing))
         "/nonexistent")
        ((pair? thing) (cons (approximate (car thing))
                             (approximate (cdr thing))))
        (else thing)))

(define (run-program program)
  "Run PROGRAM, a program-file, as the current user and return its exit
status.  Its output goes to the test log when it fails."
  (let ((file (path "program.scm")))
    (call-with-output-file file
      (lambda (port)
        (write (approximate (program-file-gexp program)) port)))
    (let* ((pipe (open-pipe* OPEN_READ "sh" "-c"
                             (string-append "guile --no-auto-compile -L "
                                            %guix-modules " " file " 2>&1")))
           (output (get-string-all pipe))
           (status (status:exit-val (close-pipe pipe))))
      (unless (zero? status)
        (display output (current-error-port)))
      status)))

(define* (supervisor #:key (install-packs? #f) (soft-reload? #t)
                     (rig-path (path "rig")) (agents 'unset))
  (car (gascity-supervisor-configurations
        (list (gascity-supervisor-configuration
               (port 8372)
               (state-directory (path "state"))
               (log-directory (path "log"))
               (cities (list (gascity-city-configuration
                              (name "town")
                              (directory (path "city"))
                              (install-packs? install-packs?)
                              (soft-reload? soft-reload?)
                              (agents agents)
                              (rigs (list (gascity-rig-configuration
                                           (name "app")
                                           (path rig-path)
                                           (prefix "ap"))))))))))))

(define (activate config)
  (run-program (gascity-supervisor-activation-program config #:chown? #f)))

(define (provision config)
  (run-program (gascity-supervisor-provision-program config)))

(test-begin "gascity-lifecycle")

(test-group "gascity-sync-packs"
  (test-equal "imports in sync are left alone"
    '(#t ("import check --city /c"))
    (let ((result (gascity-sync-packs %gc "/c" (const #t))))
      (list result (gc-calls))))

  (test-equal "drifted imports are installed"
    '(#t ("import check --city /c" "import install --city /c"))
    (begin
      (touch (path "check-fails"))
      (let ((result (gascity-sync-packs %gc "/c" (const #t))))
        (list result (gc-calls)))))

  (test-equal "a failed install is reported, never raised"
    '(#f #t)
    (begin
      (touch (path "install-fails"))
      (let* ((logged #f)
             (result (gascity-sync-packs %gc "/c"
                                         (lambda (fmt . args)
                                           (set! logged #t)))))
        (delete-file (path "check-fails"))
        (delete-file (path "install-fails"))
        (gc-calls)
        (list result logged)))))

(test-group "gascity-soft-reload"
  (test-equal "the controller is asked for an asynchronous soft reload"
    '(#t ("reload --soft --async /c"))
    (let ((result (gascity-soft-reload %gc "/c" (const #t))))
      (list result (gc-calls))))

  (test-equal "a failed reload is reported, never raised"
    #f
    (begin
      (touch (path "reload-fails"))
      (let ((result (gascity-soft-reload %gc "/c" (const #t))))
        (delete-file (path "reload-fails"))
        (gc-calls)
        result))))

(test-group "activation"
  (test-equal "a city that was never provisioned is not reloaded"
    '(0 ())
    (list (activate (supervisor #:install-packs? #t)) (gc-calls)))

  (test-assert "the site binding carries the rig path"
    (string-contains (read-file (path "city/.gc/site.toml")) (path "rig")))

  (test-assert "the site binding is written before city.toml"
    ;; A reload the new city.toml triggers must see the rig paths: gc does
    ;; not reload for a change to .gc/site.toml alone.
    (let ((source (format #f "~s"
                          (approximate
                           (program-file-gexp
                            (gascity-supervisor-activation-program
                             (supervisor) #:chown? #f))))))
      (< (string-contains source "/.gc/site.toml\")")
         (string-contains source "/city.toml\")"))))

  (test-equal "a provisioned city whose files did not change is left alone"
    '(0 ())
    (begin
      (mkdir-p (path "city/.gc/runtime"))
      (list (activate (supervisor #:install-packs? #t)) (gc-calls))))

  (test-equal "a changed running city has its imports synced, then reloads softly"
    (list 0 (list (string-append "import check --city " (path "city"))
                  (string-append "reload --soft --async "
                                 (path "city"))))
    (list (activate (supervisor #:install-packs? #t
                                #:rig-path (path "rig2")))
          (gc-calls)))

  (test-equal "imports are not touched without install-packs?"
    (list 0 (list (string-append "reload --soft --async "
                                 (path "city"))))
    (list (activate (supervisor #:rig-path (path "rig3"))) (gc-calls)))

  (test-equal "soft-reload? #f leaves the reload to gc's own watcher"
    '(0 ())
    (list (activate (supervisor #:soft-reload? #f #:rig-path (path "rig4")))
          (gc-calls)))

  (test-equal "a failing gc never fails the activation"
    0
    (begin
      (touch (path "check-fails"))
      (touch (path "install-fails"))
      (touch (path "reload-fails"))
      (let ((status (activate (supervisor #:install-packs? #t
                                          #:rig-path (path "rig5")))))
        (for-each delete-file (list (path "check-fails") (path "install-fails")
                                    (path "reload-fails")))
        (gc-calls)
        status))))

(test-group "provision"
  (delete-file-recursively (path "city"))
  (delete-file-recursively (path "state"))

  (test-equal "the first run initializes the city, then checks its imports"
    (list 0 (list (string-append "init --file " (path "city/city.toml")
                                 " --preserve-existing --no-start"
                                 " --skip-provider-readiness " (path "city"))
                  (string-append "import check --city " (path "city"))))
    (let ((config (supervisor #:install-packs? #t)))
      (activate config)
      (gc-calls)
      (list (provision config) (gc-calls))))

  (test-equal "a rerun with the same inputs still checks the imports"
    (list 0 (list (string-append "import check --city " (path "city"))))
    (list (provision (supervisor #:install-packs? #t)) (gc-calls)))

  (test-equal "drifted imports are installed on a rerun"
    (list 0 (list (string-append "import check --city " (path "city"))
                  (string-append "import install --city " (path "city"))))
    (begin
      (touch (path "check-fails"))
      (let ((status (provision (supervisor #:install-packs? #t))))
        (delete-file (path "check-fails"))
        (list status (gc-calls)))))

  (test-equal "a failed install does not fail the provision"
    0
    (begin
      (touch (path "check-fails"))
      (touch (path "install-fails"))
      (let ((status (provision (supervisor #:install-packs? #t))))
        (delete-file (path "check-fails"))
        (delete-file (path "install-fails"))
        (gc-calls)
        status)))

  (test-equal "imports are not touched without install-packs?"
    ;; The changed flag changes the inputs, so genesis runs again (and is
    ;; idempotent); the imports are left to gc.
    (list 0 (list (string-append "init --file " (path "city/city.toml")
                                 " --preserve-existing --no-start"
                                 " --skip-provider-readiness " (path "city"))))
    (list (provision (supervisor)) (gc-calls))))

(test-end "gascity-lifecycle")

(unless (getenv "GASCITY_TEST_KEEP")
  (delete-file-recursively %root))
