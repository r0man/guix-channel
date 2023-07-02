(define-module (r0man guix packages lisp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages wm)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public sbcl-cl-postgres+local-time
  (let ((commit "40169fe26d9639f3d9560ec0255789bf00b30036")
        (revision "3"))
    (package
      (name "sbcl-cl-postgres+local-time")
      (version (git-version "1.0.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dlowe-net/local-time")
               (commit commit)))
         (file-name (git-file-name "cl-postgres-local-time" version))
         (sha256
          (base32 "1dbp33zmkqzzshmf5k76pxqgli285wvy0p0dhcz816fdikpwn2jg"))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-local-time sbcl-postmodern))
      (native-inputs
       (list sbcl-hu.dwim.stefil))
      (arguments
       `(#:asd-systems '("cl-postgres+local-time")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'delete-local-time
             (lambda _
               (delete-file "local-time.asd")
               (delete-file "src/package.lisp")
               (delete-file "src/local-time.lisp")
               (delete-file-recursively "doc")
               (delete-file-recursively "test")
               (delete-file-recursively "zoneinfo"))))))
      (home-page "https://common-lisp.net/project/local-time/")
      (synopsis "Time manipulation library for Common Lisp")
      (description
       "The LOCAL-TIME library is a Common Lisp library for the manipulation of
dates and times.  It is based almost entirely upon Erik Naggum's paper \"The
Long Painful History of Time\".")
      (license license:expat))))

(define-public cl-postgres+local-time
  (sbcl-package->cl-source-package sbcl-cl-postgres+local-time))

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

(define-public sbcl-stumpwm-battery-portable
  (let ((commit "a7dc1c663d04e6c73a4772c8a6ad56a34381096a")
        (revision "3"))
    (package
      (name "sbcl-stumpwm-battery-portable")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stumpwm/stumpwm-contrib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "09akdaaya7lga5lzbq1aj1filsyjwvflghkidpmr0nk0jz5xx1g7"))))
      (build-system asdf-build-system/sbcl)
      (inputs `(("stumpwm" ,stumpwm "lib")))
      (arguments
       '(#:asd-systems '("battery-portable")
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "modeline/battery-portable") #t)))))
      (home-page "https://github.com/stumpwm/stumpwm-contrib")
      (synopsis "Integrate @code{battery-portable} with StumpWM")
      (description "Add battery information to the modeline in a portable way.")
      (license (list license:gpl2+ license:gpl3+ license:bsd-2)))))
