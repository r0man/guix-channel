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

(define-public sbcl-cl-bnf
  (let ((commit "6fb3e02c1b4039a0a81c0425c5704e60595bec6d")
        (revision "0"))
    (package
      (name "sbcl-cl-bnf")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/diasbruno/cl-bnf")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "05lskaz5kwl4sk8mb13l4kvr3q0q56dyarmznpp1cjpan6rdyr4x"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/diasbruno/cl-bnf")
      (synopsis "A simple BNF parser")
      (description "A simple BNF parser in Common Lisp")
      (license (list license:expat)))))

(define-public cl-bnf
  (sbcl-package->cl-source-package sbcl-cl-bnf))

(define-public sbcl-cl-ewkb
  (let ((commit "e2c7976cdc2074d63038ecd7179025ccb8d8f0b7")
        (revision "0"))
    (package
      (name "sbcl-cl-ewkb")
      (version (git-version "0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/filonenko-mikhail/cl-ewkb")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1n1zm4i11638vh1a4m71690p4lpikkk0rp42j2yfvs5d9wi67cr1"))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-ieee-floats sbcl-flexi-streams))
      (home-page "https://github.com/filonenko-mikhail/cl-ewkb")
      (synopsis "Common Lisp PostGIS EWKB library")
      (description "cl-ewkb is a geospatial library, based on cl-wkb, that implements the
OGC Well-Known Binary geographic geometry data model with PostGIS 3d,
4d extensions, and provides WKB and EWKB encoding and decoding
functionality.")
      (license (list license:expat)))))

(define-public cl-ewkb
  (sbcl-package->cl-source-package sbcl-cl-ewkb))

(define-public ecl-cl-ewkb
  (sbcl-package->ecl-package sbcl-cl-ewkb))

(define-public sbcl-birch
  (let ((commit "30cd24260675c6c4e276daaf28be8d02ac15dd8f")
        (revision "0"))
    (package
      (name "sbcl-birch")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jorams/birch")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1b24xng92ra7420s3zy44pybk4h7xg4kjwdk35arl46badgi28r1"))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-alexandria
                    sbcl-cl+ssl
                    sbcl-flexi-streams
                    sbcl-split-sequence
                    sbcl-usocket))
      (native-inputs (list sbcl-prove))
      (arguments '(#:asd-systems '("birch")))
      (home-page "https://github.com/jorams/birch")
      (synopsis "Common Lisp IRC client library")
      (description "Birch is a simple Common Lisp IRC client library.  It makes use of
CLOS for event handling.")
      (license (list license:expat)))))

(define-public cl-birch
  (sbcl-package->cl-source-package sbcl-birch))

(define-public ecl-birch
  (sbcl-package->ecl-package sbcl-birch))

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

(define-public sbcl-cl-str
  (package
    (name "sbcl-cl-str")
    (version "0.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vindarel/cl-str")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r9niyvkj7jyc93rxys6pgqazzpl1ybfryjn8jig721xhjxrsblm"))))
    (build-system asdf-build-system/sbcl)
    (arguments `(#:asd-systems '("str" "str.test")))
    (inputs (list sbcl-cl-ppcre
                  sbcl-cl-ppcre-unicode
                  sbcl-cl-change-case))
    (home-page "https://github.com/vindarel/cl-str")
    (synopsis " Common Lisp string manipulation library")
    (description "Modern, simple and consistent Common Lisp string manipulation library.")
    (license (list license:expat))))

(define-public cl-str
  (sbcl-package->cl-source-package sbcl-cl-str))

(define-public ecl-cl-str
  (sbcl-package->ecl-package sbcl-cl-str))

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
      (version "0.0.1")
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
      (version "0.0.1")
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
