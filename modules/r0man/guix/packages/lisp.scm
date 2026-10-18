(define-module (r0man guix packages lisp)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages zig)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system zig)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public sbcl-cl-autowrap-next
  (let ((revision "3")
        (commit "4bba9e37b59cd191dea150a89aef7245a40b1c9d"))
    ;; no tagged branches
    (package
      (name "sbcl-cl-autowrap-next")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpav/cl-autowrap")
               (commit commit)))
         (file-name (git-file-name "cl-autowrap" version))
         (sha256
          (base32 "1sfvhyrwm9dhxi0y42xp7mx8mvs6lmq3bzxdx34frxni5srcgly0"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list c2ffi))
      (inputs
       (list libffi
             sbcl-alexandria
             sbcl-cffi
             sbcl-cl-json
             sbcl-cl-ppcre
             sbcl-defpackage-plus
             sbcl-trivial-features))
      (arguments
       (list #:asd-systems ''("cl-autowrap" "cl-plus-c" "cl-autowrap/libffi")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-paths
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "autowrap-libffi/library.lisp"
                       (("libffi.so")
                        (search-input-file inputs "lib/libffi.so")))
                     (substitute* "autowrap-libffi/autowrap.lisp"
                       (("/usr/lib64/libffi-3.2.1/include")
                        (search-input-directory inputs "include"))))))))
      (home-page "https://github.com/rpav/cl-autowrap")
      (synopsis "FFI wrapper generator for Common Lisp")
      (description "This is a c2ffi-based wrapper generator for Common Lisp.")
      (license license:bsd-2))))

(define-public cl-autowrap-next
  (sbcl-package->cl-source-package sbcl-cl-autowrap-next))

(define-public sbcl-cl-state-machine
  (let ((commit "7f8d1e05f34f6906a9aa7eabb0cdeabb11ace1a5")
        (revision "1"))
    (package
      (name "sbcl-cl-state-machine")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/r0man/cl-state-machine")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0fh1f5fw87qlcz2vcdbn79wijzwksncz70sxw9wn0srg3m96fa0r"))))
      (build-system asdf-build-system/sbcl)
      (arguments '(#:asd-systems '("cl-state-machine"
                                   "cl-state-machine-examples"
                                   "cl-state-machine-graphing"
                                   "cl-state-machine-test")))
      (native-inputs (list sbcl-fiveam))
      (home-page "https://github.com/ageldama/cl-state-machine")
      (synopsis "State machine DSL/library for Common Lisp")
      (description "Simple state machine DSL/library for Common Lisp.")
      (license (list license:expat)))))

(define-public cl-state-machine
  (sbcl-package->cl-source-package sbcl-cl-state-machine))

(define-public ecl-cl-state-machine
  (sbcl-package->ecl-package sbcl-cl-state-machine))

(define-public sbcl-fsm
  (let ((commit "d528569982e1d8e4355066d38e1383fd09ddc968")
        (revision "1"))
    (package
      (name "sbcl-fsm")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.sr.ht/~hdasch/fsm")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0wx6a9mrhhklbyiskgn18bw0pj9sz7cd4r4cb5773xb2pddrs1a2"))))
      (build-system asdf-build-system/sbcl)
      (arguments (list #:tests? #f)) ;; Tests require network access
      (inputs (list sbcl-alexandria
                    sbcl-babel
                    sbcl-cl-async
                    sbcl-log4cl
                    sbcl-serapeum))
      (native-inputs (list sbcl-fiveam))
      (home-page "https://git.sr.ht/~hdasch/fsm")
      (synopsis "State machine library for Common Lisp")
      (description "This package provides a port of Magnus Henoch's Emacs Lisp finite
state machine (fsm.el) to Common Lisp.  Its primary use is managing
network connections and transaction on a network socket.")
      (license (list license:gpl3+)))))

(define-public cl-fsm
  (sbcl-package->cl-source-package sbcl-fsm))

(define-public ecl-fsm
  (sbcl-package->ecl-package sbcl-fsm))

(define-public sbcl-cl-hash-util
  (let ((commit "7f88cb7579b2af8c21022554f46dddd6ce6a5fc2")
        (revision "1"))
    (package
      (name "sbcl-cl-hash-util")
      (version "0.0.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orthecreedence/cl-hash-util")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1xab7v2mav241rs8w68qmg485g4f75nrac3hjcnm0cb19ickbs1m"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs (list sbcl-fiveam))
      (home-page "https://github.com/orthecreedence/cl-hash-util")
      (synopsis "Small hash utility library for Common Lisp")
      (description "This package provides a basic library for dealing with Common Lisp's
hash tables.")
      (license (list license:expat)))))

(define-public cl-hash-util
  (sbcl-package->cl-source-package sbcl-cl-hash-util))

(define-public ecl-cl-hash-util
  (sbcl-package->ecl-package sbcl-cl-hash-util))

(define-public sbcl-json-mop
  (let ((commit "66705776dec7056fabba09646d726782baf019d9")
        (revision "1"))
    (package
      (name "sbcl-json-mop")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gschjetne/json-mop")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1q6mmq64hf4v448bnzfh8nxsxg5h18a9snh785r1fnvv1aij3fi1"))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-anaphora
                    sbcl-closer-mop
                    sbcl-yason))
      (home-page "https://github.com/gschjetne/json-mop")
      (synopsis "Metaclass for bridging CLOS and JSON objects")
      (description "JSON-MOP is a small library aiming to cut down time spent moving data
between CLOS and JSON objects. It depends on YASON and it should be
possible to use it alongside straight calls to functions from YASON.")
      (license (list license:expat)))))

(define-public json-mop
  (sbcl-package->cl-source-package sbcl-json-mop))

(define-public ecl-json-mop
  (sbcl-package->ecl-package sbcl-json-mop))

(define-public sbcl-openapi-generator
  (let ((commit "28c399154c5589fd3d4303dfe4725243349cfdb7")
        (revision "1"))
    (package
      (name "sbcl-openapi-generator")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/kilianmh/openapi-generator")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zc0y8frcnsqj76sqmqsgfv0zhdz5kkpynwan3sigc78fl1nrs3q"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-module-load
              (lambda* (#:key outputs #:allow-other-keys)
                (substitute* "code/collections.lisp"
                  (("\\(get-api-guru-apis\\)")
                   "(make-hash-table)")))))))
      (inputs (list sbcl-alexandria
                    sbcl-cl-hash-util
                    sbcl-cl-json-pointer
                    sbcl-cl-project
                    sbcl-cl-semver
                    sbcl-cl-str
                    sbcl-cl-yaml
                    sbcl-dexador
                    sbcl-json-mop
                    sbcl-jzon
                    sbcl-listopia
                    sbcl-moptilities
                    sbcl-parse-float
                    sbcl-pathname-utils
                    sbcl-quri
                    sbcl-serapeum
                    sbcl-yason))
      (native-inputs (list sbcl-fiveam))
      (home-page "https://codeberg.org/kilianmh/openapi-generator")
      (synopsis "Common Lisp OpenAPI Generator")
      (description "This package provides an OpenAPI client system generator.")
      (license (list license:gpl3+)))))

(define-public openapi-generator
  (sbcl-package->cl-source-package sbcl-openapi-generator))

(define-public ecl-openapi-generator
  (sbcl-package->ecl-package sbcl-openapi-generator))

(define-public sbcl-cacle
  (let ((commit "4cbe8cfe227d2e097eaced14766f4f37aa05e617")
        (revision "1"))
    (package
      (name "sbcl-cacle")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jlahd/cacle")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0h0dk0sfkfl8g0sbrs76ydb9l4znssqhx8nc5k1sg7zxpni5a4qy"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-bordeaux-threads))
      (home-page "https://github.com/jlahd/cacle")
      (synopsis "Extensible cache services for Common Lisp")
      (description "This package provides implements a generic cache management facility
with configurable and extensible cache replacement policies.  The
actual cached data can be stored anywhere, with cacle taking charge of
keeping track of which entry is to be discarded next when more space
is needed for a new entry.")
      (license (list license:expat)))))

(define-public cl-cacle
  (sbcl-package->cl-source-package sbcl-cacle))

(define-public ecl-cacle
  (sbcl-package->ecl-package sbcl-cacle))

(define-public sbcl-clx-truetype-stumpwm
  (let ((commit "2378039ada60f1e7a338a7c00d66f8513e3b91ab")
        (revision "2"))
    (package
      (name "sbcl-clx-truetype")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stumpwm/clx-truetype")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0gab24gc0ghcbyi2frylpns51jv1mvypyff0wwq48rzww5gx7fyc"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (substitute* "package.lisp"
               ((":export") ":export\n   :+font-cache-filename+"))
             #t))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-clx
             sbcl-cacle
             sbcl-zpb-ttf
             sbcl-cl-vectors
             sbcl-cl-fad
             sbcl-cl-store
             sbcl-trivial-features))
      (home-page "https://github.com/stumpwm/clx-truetype")
      (synopsis "Antialiased TrueType font rendering using CLX and XRender")
      (description "CLX-TrueType is pure common lisp solution for
antialiased TrueType font rendering using CLX and XRender extension.")
      (license license:expat))))

(define-public cl-clx-truetype-stumpwm
  (sbcl-package->cl-source-package sbcl-clx-truetype-stumpwm))

(define-public ecl-clx-truetype
  (sbcl-package->ecl-package sbcl-clx-truetype-stumpwm))

(define-public sbcl-rove-next
  ;; Guix's sbcl-rove is pinned to a 2023 snapshot that predates the
  ;; ASSERTION-SOURCE-LOCATION reader, which cl-mcp's test runner requires.
  (let ((commit "52b04eaa4a079d753fdf49e793a52d5fe1f61836")
        (revision "2"))
    (package
      (inherit sbcl-rove)
      (name "sbcl-rove-next")
      (version (git-version "0.10.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/rove")
               (commit commit)))
         (file-name (git-file-name "cl-rove" version))
         (sha256
          (base32 "03km3ykp7h2skny76vqj48806cildv2f36gwqv6x1bbcyagirzgs"))))
      ;; The system is still called "rove"; only the Guix package is renamed.
      (arguments (list #:asd-systems ''("rove"))))))

(define-public cl-rove-next
  (sbcl-package->cl-source-package sbcl-rove-next))

(define-public sbcl-yason-next
  ;; YASON:FALSE only became a variable after the 0.8.5 release, and cl-mcp
  ;; evaluates it throughout its sources and tests.
  (let ((commit "0c84b29a32c284f6b4be6f8587ff89b5702a6fda")
        (revision "0"))
    (package
      (inherit sbcl-yason)
      (name "sbcl-yason-next")
      (version (git-version "0.8.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/phmarek/yason")
               (commit commit)))
         (file-name (git-file-name "cl-yason" version))
         (sha256
          (base32 "1bbkqsd7qfih089zs1fpkq8lb4z6xgjj8qswhwd8kgb5wax78lbn"))))
      ;; The system is still called "yason"; only the Guix package is renamed.
      (arguments
       (list
        #:asd-systems ''("yason")))
      (native-inputs (list sbcl-unit-test)))))

(define-public cl-yason-next
  (sbcl-package->cl-source-package sbcl-yason-next))

(define-public sbcl-cl-mcp
  (package
    (name "sbcl-cl-mcp")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cl-ai-project/cl-mcp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1arqh2895paa7j6df84x44y6bb3gd2gblidvv7jpk687hxlsgp6n"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     (list
      #:asd-systems ''("cl-mcp")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'require-asdf-in-worker
            ;; Workers are spawned with bare SBCL, which lacks ASDF.
            (lambda _
              (substitute* "src/worker-client.lisp"
                (("\\(list \"--noinform\" \"--non-interactive\"")
                 "(list \"--noinform\" \"--non-interactive\"
           \"--eval\" \"(require :asdf)\""))))
          (add-after 'unpack 'patch-test-shell
            ;; The build container has no /bin/sh.
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (substitute* "tests/worker-leaked-thread-test.lisp"
                (("\"/bin/sh\"")
                 (string-append "\""
                                (search-input-file (or native-inputs inputs)
                                                   "/bin/sh") "\"")))))
          (add-after 'unpack 'disable-store-specific-tests
            ;; These tests fail against the read-only store: they reload
            ;; or write into the installed sources, rely on source write
            ;; dates (reset to the epoch), or expect the project root to
            ;; be the current directory.
            (lambda _
              (define (disable file . tests)
                (for-each (lambda (test)
                            (substitute* file
                              (((string-append "^\\(deftest " test))
                               (string-append "#+(or) (deftest " test))))
                          tests))
              (disable "tests/system-loader-test.lisp"
               "load-system-warning-fields"
               "load-system-clear-fasls-on-a-subsystem-clears-its-primary")
              (disable "tests/worker-test.lisp"
                       "spawn-worker-includes-child-stderr-in-error"
                       ;; Exhausts the stack outside the parser's handler
                       ;; and kills the test process.
                       "worker-answers-a-deeply-nested-line")
              (disable "tests/code-test.lisp"
               "definition-source-location-reports-path-line-and-staleness"
               "code-find-references-does-not-place-a-stale-reference"
               "code-find-definition-says-when-its-line-predates-the-file")
              ;; Its fixture reads a form under #+x86-64.
              (disable "tests/lisp-edit-form-test.lisp"
                       "lisp-edit-form-replace-keeps-the-feature-expression")
              (disable "tests/clos-describe-integration-test.lisp"
               "clos-describe-agrees-across-pool-and-no-pool-paths"
               "clos-describe-matches-a-quoted-eql-keyword-over-the-worker-rpc"
               "clos-describe-hands-out-an-edit-guard"
               "clos-describe-does-not-confirm-an-accessor-against-a-writer-only-slot"
               "clos-describe-points-a-condition-reader-override-at-its-own-defmethod")
              (disable "tests/clos-response-builders-test.lisp"
               "annotate-report-forms-reports-a-real-mismatch"
               "annotate-report-forms-falls-back-when-the-edit-tool-cannot-locate-it-uniquely")
              (disable "tests/fs-test.lisp"
                       "fs-set-project-root-converts-relative-to-absolute"
                       "fs-set-project-root-relative-path-subdirectory")))
          (add-after 'unpack 'relocate-throwaway-project-root
            ;; The build container has no /var/tmp, which tests use as a
            ;; throwaway project root.
            (lambda _
              (let ((root (string-append (getcwd) "/.test-project-root")))
                (mkdir-p root)
                (substitute* (list "tests/pool-test.lisp"
                                   "tests/utils-paths-test.lisp"
                                   "tests/worker-test.lisp")
                  (("/var/tmp")
                   root)))))
          (add-after 'unpack 'fail-on-test-failure
            ;; TEST-OP ignores ROVE:RUN's result, so failures pass.
            (lambda _
              (substitute* "tests.lisp"
                (("\\(rove:run (quick|\\(append [^)]*\\))\\)" all)
                 (string-append "(or " all
                                " (error \"cl-mcp test suite failed\"))")))))
          (add-before 'check 'run-all-test-tiers
            ;; The default quick tier skips the worker and server suites.
            (lambda _
              (setenv "CL_MCP_TEST_TIER" "full"))))))
    (inputs (list sbcl-alexandria
                  sbcl-bordeaux-threads
                  sbcl-cl-ppcre
                  sbcl-eclector
                  sbcl-hunchentoot
                  sbcl-named-readtables
                  sbcl-usocket
                  sbcl-yason-next))
    (native-inputs (list sbcl-fiveam sbcl-rove-next))
    (home-page "https://github.com/cl-ai-project/cl-mcp")
    (synopsis "Model Context Protocol server for Common Lisp")
    (description
     "This package provides a Model Context Protocol (MCP) server for Common
Lisp, speaking JSON-RPC 2.0 over stdio, TCP and Streamable HTTP.  It lets AI
agents drive a live Common Lisp environment through structured tools: REPL
evaluation with object inspection and stack frame capture, sandboxed file
operations restricted to the project root, structure-aware editing of
top-level forms via Eclector concrete syntax trees with parinfer repair,
symbol lookup and cross-references, HyperSpec lookup, and a structured test
runner for Rove and FiveAM suites.  Evaluation happens in isolated child SBCL
worker processes with crash recovery and per-session affinity.")
    (license license:expat)))

(define-public cl-mcp
  (sbcl-package->cl-source-package sbcl-cl-mcp))

(define-public parenmedic
  (let ((revision "0")
        (commit "e24884e50837d5c7a85c89997b49daee52003345"))
    (package
      (name "parenmedic")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/shcv/parenmedic")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03kjh94d1k9hw6rdrgrw6p4s27hknlq1a4kdaawvs0ax4l34pzv4"))))
      (build-system zig-build-system)
      (arguments
       (list
        #:zig zig-0.14
        #:zig-release-type "safe"
        #:install-source? #f))
      (home-page "https://github.com/shcv/parenmedic")
      (synopsis "Diagnose and fix parenthesis issues in Lisp code")
      (description
       "Parenmedic is a command-line tool that uses indentation as a guide to
locate and repair structural problems in Lisp source files: missing closing
parentheses, extra closing delimiters and mismatched delimiter types such as
@samp{]} where @samp{)} is expected.  Issues can be reported or fixed in
place, with a diff, or on standard output, and Common Lisp, Scheme, Clojure,
Emacs Lisp, Janet and Racket are recognized from the file extension.
Diagnostics are available as colored context, one line per issue, in a
@command{gcc}-compatible form for editors, as JSON, or as Language Server
Protocol data.  Parenmedic can also run as a stdio @acronym{MCP, Model Context
Protocol} server so that language models can call its @code{diagnose} and
@code{repair} tools.")
      (license license:cc0))))
