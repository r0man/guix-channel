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
