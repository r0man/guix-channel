(define-module (r0man guix packages golang-maths)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public go-github-com-remyoudompheng-bigfft
  (package
    (name "go-github-com-remyoudompheng-bigfft")
    (version "0.0.0-20230129092748-24d4a6f8daec")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/remyoudompheng/bigfft")
             (commit "24d4a6f8daec97c3999c0ad7457c0a8cc27a6f3c")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qxfda0jq70ank99zlgfz7iig2jpicbbxnpr7xcf1v9p474ak2dx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/remyoudompheng/bigfft"))
    (home-page "https://github.com/remyoudompheng/bigfft")
    (synopsis "FFT-based multiplication of big integers")
    (description
     "This package provides big integer multiplication using FFT.")
    (license license:bsd-3)))

(define-public go-modernc-org-mathutil
  (package
    (name "go-modernc-org-mathutil")
    (version "1.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/mathutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09yhqhyaq5ikqm8afj09vr1v1ji7lwvd16mysr8hb1c6bnn7krh8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/mathutil"
      #:unpack-path "modernc.org/mathutil"))
    (propagated-inputs
     (list go-github-com-remyoudompheng-bigfft))
    (home-page "https://modernc.org/mathutil")
    (synopsis "Math utilities for Go")
    (description
     "Package mathutil provides utilities supplementing the standard library
math and big packages.")
    (license license:bsd-3)))
