(define-module (r0man guix packages lisp)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages wm)
  #:use-module (guix build-system)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public sbcl-cl-autowrap-next
  (let ((revision "2")
        (commit "1e6735ae8e22bd8805b89a1857417756bb8a0f31"))
    ;; no taged branches
    (package
      (name "sbcl-cl-autowrap")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpav/cl-autowrap")
               (commit commit)))
         (file-name (git-file-name "cl-autowrap" version))
         (sha256
          (base32 "0pbabpmg61bflx6kxllqvhbvxqwjsik3nnynqdhgzzkgzk6jlixv"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems '("cl-plus-c" "cl-autowrap")))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("cl-json" ,sbcl-cl-json)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("defpackage-plus" ,sbcl-defpackage-plus)
         ("trivial-features" ,sbcl-trivial-features)))
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

(define-public sbcl-cl-semver
  (let ((commit "b125d2c49ea6d370302dde73a6e0841c0e928184")
        (revision "1"))
    (package
      (name "sbcl-cl-semver")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cldm/cl-semver")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zlcn7lrpvjiixgqm4yxnqqwak1hxfmxmchkpvrly41yhl586ril"))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-alexandria
                    sbcl-esrap
                    sbcl-named-readtables))
      (native-inputs (list sbcl-stefil))
      (home-page "https://github.com/cldm/cl-semver")
      (synopsis "Semantic Versions handling in Common Lisp")
      (description "This package provides a Common Lisp implementation of the
Semantic Versioning Specification.")
      (license (list license:expat)))))

(define-public cl-semver
  (sbcl-package->cl-source-package sbcl-cl-semver))

(define-public ecl-cl-semver
  (sbcl-package->ecl-package sbcl-cl-semver))

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
