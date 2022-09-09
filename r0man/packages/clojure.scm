(define-module (r0man packages clojure)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public clojure-lsp
  (package
    (name "clojure-lsp")
    (version "2022.09.01-15.27.31")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/clojure-lsp/clojure-lsp/releases/download/"
             version
             "/clojure-lsp-native-static-linux-amd64.zip"))
       (sha256
        (base32 "04yiwdrq30i8cl3gwj5sga9r9flzyc3j0wh3q3p567f8r3a6drsy"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (arguments
     `(#:install-plan
       '(("clojure-lsp" "bin/"))))
    (home-page "https://clojure.org/releases/tools")
    (synopsis "CLI tools for the Clojure programming language")
    (description "The Clojure command line tools can be used to start a
Clojure repl, use Clojure and Java libraries, and start Clojure programs.")
    (license license:epl1.0)))
