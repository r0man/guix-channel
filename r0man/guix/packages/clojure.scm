(define-module (r0man packages clojure)
  #:use-module (gnu packages gcc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public babashka
  (package
    (name "babashka")
    (version "1.0.168")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/babashka/babashka/releases/download/v"
                                  version "/babashka-" version "-linux-amd64-static.tar.gz"))
              (sha256
               (base32
                "126sx9q9lsm4s4g5xplndw4difq9rbp7r633ky9j2x57sa11cbf7"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("bb" "bin/"))))
    (synopsis "Native, fast starting Clojure interpreter for scripting")
    (description
     "Babashka is a native Clojure interpreter for scripting with fast
startup. Its main goal is to leverage Clojure in places where you
would be using bash otherwise.")
    (home-page "https://babashka.org")
    (license license:epl1.0)))

(define-public clojure-lsp
  (package
    (name "clojure-lsp")
    (version "2022.12.09-15.51.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/clojure-lsp/clojure-lsp/releases/download/"
             version
             "/clojure-lsp-native-static-linux-amd64.zip"))
       (sha256
        (base32 "1l0dbk5samqgjr5jyh6djqmrh5pi2bs7l6dkkqhqs43qq5df0xsg"))))
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
