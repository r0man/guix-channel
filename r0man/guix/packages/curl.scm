(define-module (r0man guix packages curl)
  #:use-module ((gnu packages curl) #:prefix curl:)
  #:use-module (gnu packages tls)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public curl-with-openssl
  (package
    (inherit curl:curl)
    (name "curl-with-openssl")
    (arguments
     (substitute-keyword-arguments (package-arguments curl:curl)
       ((#:configure-flags flags)
        `(list "--disable-static"
               "--with-openssl"))))
    (inputs
     (modify-inputs (package-inputs curl:curl)
       (delete "gnutls")
       (prepend openssl)))))
