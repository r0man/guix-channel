(define-module (r0man packages container)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public container-structure-test
  (package
    (name "container-structure-test")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://storage.googleapis.com/container-structure-test/v"
             version
             "/container-structure-test-linux-amd64"))
       (sha256
        (base32 "16qqrswmkxnm1lw899p60zgkbwa5bbh3syi5r728iz50smgxl90m"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (arguments
     `(#:install-plan
       '(("container-structure-test-linux-amd64" "bin/container-structure-test"))
       #:phases
         (modify-phases %standard-phases
           (add-before 'install 'set-permissions
             (lambda _
               (chmod "container-structure-test-linux-amd64" #o755))))))
    (home-page "https://clojure.org/releases/tools")
    (synopsis "Container Structure Tests")
    (description "The Container Structure Tests provide a powerful framework to
validate the structure of a container image. These tests can be used
to check the output of commands in an image, as well as verify
metadata and contents of the filesystem.")
    (license license:asl2.0)))
