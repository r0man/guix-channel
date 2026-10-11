;;; Unit tests for the `files' and `rig-files' of a Gas City city: the
;;; validation of their entries, and what the activation program does with
;;; them across generations -- single files, directory trees and symbolic
;;; links are installed, an unchanged generation leaves them alone, a
;;; read-only copy is replaced, and what a later generation no longer
;;; declares is removed (together with the dangling formula links gc staged
;;; for it) while unmanaged files are kept.
;;;
;;; The activation program runs as the current user, the way the Home
;;; service runs it (`#:chown? #f'), against a temporary directory.  Sources
;;; are plain file names, so `gexp->approximate-sexp' keeps them and no store
;;; or daemon is needed.

(define-module (test-r0man-guix-services-gascity-files)
  #:use-module (guix build utils)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (r0man guix services gascity)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports))

(define %root
  (let ((directory (string-append (or (getenv "TMPDIR") "/tmp")
                                  "/gascity-files-test-"
                                  (number->string (getpid)))))
    (when (file-exists? directory)
      (delete-file-recursively directory))
    (mkdir-p directory)
    directory))

(define (path . components)
  (string-join (cons %root components) "/"))

(define (write-file file content . mode)
  (mkdir-p (dirname file))
  (when (file-exists? file)
    (chmod file #o644))
  (call-with-output-file file (lambda (port) (display content port)))
  (chmod file (if (pair? mode) (car mode) #o644)))

(define (read-file file)
  (call-with-input-file file get-string-all))

(define (mode file)
  (stat:perms (lstat file)))

(define (file-type file)
  (let ((st (false-if-exception (lstat file))))
    (and st (stat:type st))))

(define %guix-modules
  ;; The directory holding (guix build utils), the program's only import.
  (let ((utils (search-path %load-path "guix/build/utils.scm")))
    (dirname (dirname (dirname utils)))))

(define* (city-config #:key (files '()) (rig-files '()))
  (gascity-supervisor-configuration
   (port 8372)
   (state-directory (path "state"))
   (log-directory (path "log"))
   (cities (list (gascity-city-configuration
                  (name "town")
                  (directory (path "city"))
                  (rigs (list (gascity-rig-configuration
                               (name "app")
                               (path (path "rig"))
                               (prefix "ap"))))
                  (files files)
                  (rig-files rig-files))))))

(define (approximate thing)
  "Return the approximate S-expression of THING, a gexp, with every nested
gexp approximated too (`gexp->approximate-sexp' leaves those as objects) and
every store item it cannot approximate (the `gc' executable, the packages on
PATH) replaced by a file name that does not exist."
  (cond ((gexp? thing) (approximate (gexp->approximate-sexp thing)))
        ((or (equal? thing '(*approximate*))
             (file-like? thing)
             (file-append? thing))
         "/nonexistent")
        ((pair? thing) (cons (approximate (car thing))
                             (approximate (cdr thing))))
        (else thing)))

(define (activate config)
  "Validate CONFIG like the service does, run its activation program as the
current user and return its exit status."
  (let ((config (car (gascity-supervisor-configurations (list config))))
        (file (path "activation.scm")))
    (call-with-output-file file
      (lambda (port)
        (write (approximate
                (program-file-gexp
                 (gascity-supervisor-activation-program config
                                                        #:chown? #f)))
               port)))
    (let* ((pipe (open-pipe* OPEN_READ "sh" "-c"
                             (string-append "guile --no-auto-compile -L "
                                            %guix-modules " " file " 2>&1")))
           (output (get-string-all pipe))
           (status (status:exit-val (close-pipe pipe))))
      (unless (zero? status)
        (display output (current-error-port)))
      status)))

(define (error-message thunk)
  "Call THUNK and return the text of the formatted message it raises, or #f
if it returns normally."
  (guard (condition ((formatted-message? condition)
                     (apply format #f
                            (formatted-message-string condition)
                            (formatted-message-arguments condition))))
    (thunk)
    #f))

(define (rejects? files)
  (error-message
   (lambda ()
     (gascity-supervisor-configurations (list (city-config #:files files))))))

;; The sources.
(write-file (path "src/a.md") "A\n")
(write-file (path "src/tree/bin/run.sh") "#!/bin/sh\n" #o555)
(write-file (path "src/tree/notes/z.md") "Z\n" #o444)
(write-file (path "src/tree/formulas/f.toml") "formula = 1\n" #o444)
(write-file (path "src/check.sh") "#!/bin/sh\nexit 0\n" #o555)
(write-file (path "src/rigtree/hooks/r.md") "R\n")
(write-file (path "src/rigtree/pack.toml") "[pack]\n")

(define %generation-1
  (city-config
   #:files `(("prompts/a.md" . ,(path "src/a.md"))
             ("pack" . ,(path "src/tree"))
             ("scripts/helper.py" . ,(gascity-symlink "/opt/helper.py")))
   #:rig-files `((".gc/scripts/checks/check.sh" . ,(path "src/check.sh"))
                 ("." . ,(path "src/rigtree")))))

(test-begin "gascity-files")

(test-group "validation"
  (test-assert "a relative destination and a file name source are accepted"
    (not (rejects? `(("a/b.md" . ,(path "src/a.md"))))))

  (test-assert "\".\" names the root itself"
    (not (rejects? `(("." . ,(path "src/tree"))))))

  (test-assert "a symbolic link entry is accepted"
    (not (rejects? `(("a" . ,(gascity-symlink "/x"))
                     ("b" . ,(gascity-symlink (plain-file "b" "B")))))))

  (test-assert "a destination escaping the root is rejected"
    (string-contains (rejects? `(("../x" . ,(path "src/a.md"))))
                     "'..' components"))

  (test-assert "an absolute destination is rejected"
    (rejects? `(("/etc/passwd" . ,(path "src/a.md")))))

  (test-assert "an empty component is rejected"
    (rejects? `(("a//b" . ,(path "src/a.md")))))

  (test-assert "a destination the service writes itself is rejected"
    (string-contains (rejects? `(("city.toml" . ,(path "src/a.md"))))
                     "written by the service"))

  (test-assert "a duplicate destination is rejected"
    (string-contains (rejects? `(("a" . ,(path "src/a.md"))
                                 ("a" . ,(path "src/check.sh"))))
                     "more than once"))

  (test-assert "a relative file name source is rejected"
    (rejects? `(("a" . "src/a.md"))))

  (test-assert "an empty symbolic link target is rejected"
    (rejects? `(("a" . ,(gascity-symlink "")))))

  (test-assert "rig-files are validated too"
    (error-message
     (lambda ()
       (gascity-supervisor-configurations
        (list (city-config #:rig-files `(("../x" . ,(path "src/a.md"))))))))))

(test-group "installation"
  (test-equal "the first generation activates"
    0
    (activate %generation-1))

  (test-equal "a single file is copied"
    "A\n"
    (read-file (path "city/prompts/a.md")))

  (test-equal "a plain file is 0644"
    #o644
    (mode (path "city/prompts/a.md")))

  (test-equal "a directory source installs every file below the destination"
    '("#!/bin/sh\n" "Z\n" "formula = 1\n")
    (map read-file (list (path "city/pack/bin/run.sh")
                         (path "city/pack/notes/z.md")
                         (path "city/pack/formulas/f.toml"))))

  (test-equal "an executable source is installed 0755, a read-only one 0644"
    '(#o755 #o644)
    (map mode (list (path "city/pack/bin/run.sh")
                    (path "city/pack/notes/z.md"))))

  (test-equal "a symbolic link entry becomes a link to its target"
    "/opt/helper.py"
    (readlink (path "city/scripts/helper.py")))

  (test-equal "rig-files are installed into each rig root"
    "#!/bin/sh\nexit 0\n"
    (read-file (path "rig/.gc/scripts/checks/check.sh")))

  (test-equal "the city manifest lists every managed path, sorted"
    (string-append "pack/bin/run.sh\npack/formulas/f.toml\npack/notes/z.md\n"
                   "prompts/a.md\nscripts/helper.py\n")
    (read-file (path "city/.gc/gascity-files.manifest")))

  (test-equal "a directory source at \".\" installs into the root itself"
    "R\n"
    (read-file (path "rig/hooks/r.md")))

  (test-assert "a file of a directory source never lands on a reserved path"
    (not (file-type (path "rig/pack.toml"))))

  (test-equal "the rig manifest lists its managed paths"
    ".gc/scripts/checks/check.sh\nhooks/r.md\n"
    (read-file (path "rig/.gc/gascity-files.manifest")))

  (test-assert "an unchanged generation leaves the files alone"
    (let ((before (stat (path "city/prompts/a.md"))))
      (and (zero? (activate %generation-1))
           (let ((after (stat (path "city/prompts/a.md"))))
             (and (= (stat:ino before) (stat:ino after))
                  (= (stat:mtime before) (stat:mtime after)))))))

  (test-equal "a changed source replaces a read-only copy"
    "A2\n"
    (begin
      (chmod (path "city/prompts/a.md") #o444)
      (write-file (path "src/a.md") "A2\n")
      (activate %generation-1)
      (read-file (path "city/prompts/a.md"))))

  (test-equal "a drifted mode is repaired"
    #o755
    (begin
      (chmod (path "city/pack/bin/run.sh") #o600)
      (activate %generation-1)
      (mode (path "city/pack/bin/run.sh"))))

  (test-equal "a link pointing elsewhere is redirected"
    "/opt/helper.py"
    (begin
      (delete-file (path "city/scripts/helper.py"))
      (symlink "/elsewhere" (path "city/scripts/helper.py"))
      (activate %generation-1)
      (readlink (path "city/scripts/helper.py"))))

  (test-equal "a link at a file destination is replaced by the file"
    '(regular "A2\n")
    (begin
      (delete-file (path "city/prompts/a.md"))
      (symlink (path "src/check.sh") (path "city/prompts/a.md"))
      (activate %generation-1)
      (list (file-type (path "city/prompts/a.md"))
            (read-file (path "city/prompts/a.md"))))))

(test-group "removal"
  ;; gc's staged formula links: one to a city file the next generation
  ;; retires, one dangling link the service did not cause.
  (mkdir-p (path "city/.beads/formulas"))
  (symlink (path "city/pack/formulas/f.toml")
           (path "city/.beads/formulas/f.toml"))
  (symlink (path "city/gone.toml") (path "city/.beads/formulas/gone.toml"))
  (mkdir-p (path "rig/.beads/formulas"))
  (symlink (path "city/pack/formulas/f.toml")
           (path "rig/.beads/formulas/f.toml"))
  ;; An unmanaged file next to the managed ones.
  (write-file (path "city/pack/notes/mine.md") "mine\n")

  (test-equal "the next generation activates"
    0
    (begin
      (delete-file (path "src/tree/formulas/f.toml"))
      (delete-file (path "src/tree/notes/z.md"))
      (activate
       (city-config
        #:files `(("prompts/a.md" . ,(path "src/a.md"))
                  ("pack" . ,(path "src/tree")))))))

  (test-assert "files the generation no longer declares are removed"
    (not (or (file-type (path "city/pack/notes/z.md"))
             (file-type (path "city/pack/formulas/f.toml")))))

  (test-assert "a link the generation no longer declares is removed"
    (not (file-type (path "city/scripts/helper.py"))))

  (test-assert "emptied directories are pruned"
    (not (or (file-type (path "city/scripts"))
             (file-type (path "city/pack/formulas")))))

  (test-equal "unmanaged files are kept, with their directory"
    "mine\n"
    (read-file (path "city/pack/notes/mine.md")))

  (test-equal "declared files stay"
    "#!/bin/sh\n"
    (read-file (path "city/pack/bin/run.sh")))

  (test-assert "formula links to a removed file are removed, city and rig"
    (not (or (file-type (path "city/.beads/formulas/f.toml"))
             (file-type (path "rig/.beads/formulas/f.toml")))))

  (test-equal "other dangling formula links are left alone"
    'symlink
    (file-type (path "city/.beads/formulas/gone.toml")))

  (test-equal "the manifest follows the generation"
    "pack/bin/run.sh\nprompts/a.md\n"
    (read-file (path "city/.gc/gascity-files.manifest")))

  (test-assert "a rig with no rig-files left loses its files and manifest"
    (not (or (file-type (path "rig/.gc/scripts/checks/check.sh"))
             (file-type (path "rig/hooks"))
             (file-type (path "rig/.gc/gascity-files.manifest")))))

  (test-assert "a manifest entry outside the root is ignored"
    (begin
      (write-file (path "outside.txt") "keep\n")
      (call-with-output-file (path "city/.gc/gascity-files.manifest")
        (lambda (port)
          (display "pack/bin/run.sh\nprompts/a.md\n../outside.txt\n" port)))
      (and (zero? (activate
                   (city-config
                    #:files `(("prompts/a.md" . ,(path "src/a.md"))
                              ("pack" . ,(path "src/tree"))))))
           (file-exists? (path "outside.txt")))))

  (test-equal "a directory in place of a file is never replaced"
    '(0 directory)
    (begin
      (delete-file (path "city/prompts/a.md"))
      (mkdir (path "city/prompts/a.md"))
      (list (activate
             (city-config #:files `(("prompts/a.md" . ,(path "src/a.md")))))
            (file-type (path "city/prompts/a.md"))))))

(test-end "gascity-files")

(unless (getenv "GASCITY_TEST_KEEP")
  (delete-file-recursively %root))
