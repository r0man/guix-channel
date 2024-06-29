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
