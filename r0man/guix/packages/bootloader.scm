(define-module (r0man guix packages bootloader)
  #:use-module (gnu packages gcc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages))

(define-public gnu-head-bootlogo
  (package
    (name "gnu-head-bootlogo")
    (version "0.0.1")
    (source (local-file "../files/gnu-head.svg"))
    (build-system copy-build-system)
    (home-page "https://www.gnu.org/graphics/heckert_gnu.html")
    (synopsis "GNU Head image")
    (description "A modified version of the GNU Head image")
    (license license:expat)))
