(define-module (r0man guix packages clojure)
  #:use-module (gnu packages gcc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public babashka
  (package
    (name "babashka")
    (version "1.3.179")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/babashka/babashka/releases/download/v"
                                  version "/babashka-" version "-linux-amd64-static.tar.gz"))
              (sha256
               (base32
                "0029cim2g9rfxxkdrqya8i1r5lmz02f34hnmix67cznd3qd9xlyp"))))
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
    (version "2023.05.04-19.38.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/clojure-lsp/clojure-lsp/releases/download/"
             version
             "/clojure-lsp-native-static-linux-amd64.zip"))
       (sha256
        (base32 "1zycljfrxaxicbjbpqjqrxdrzp2m6cn3my5p280fb3j5ynmjs54v"))))
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
