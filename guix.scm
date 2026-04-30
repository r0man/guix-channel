(use-modules
 ((guix licenses) #:prefix license:)
 (gnu packages autotools)
 (gnu packages gettext)
 (gnu packages guile)
 (gnu packages package-management)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (guix build-system gnu)
 (guix gexp)
 (guix packages)
 (srfi srfi-1))

(define-public guile-r0man-channel
  (package
    (name "guile-r0man-channel")
    (version "0.1")
    (source
     (local-file
      (dirname (current-filename))
      #:recursive? #t
      #:select?
      (λ (file stat)
        (not (any (lambda (s) (string-contains file s))
                  (list ".git" ".dir-locals.el" "guix.scm"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           gnu-gettext
           guile-next
           guix
           pkg-config
           texinfo))
    (inputs (list guile-next))
    (synopsis "r0man Guix channel")
    (description "Personal Guix channel by r0man.")
    (home-page "https://github.com/r0man/guix-channel")
    (license license:gpl3+)))

guile-r0man-channel
