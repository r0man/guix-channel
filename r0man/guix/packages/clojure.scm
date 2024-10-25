(define-module (r0man guix packages clojure)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages readline)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nonguix build-system binary))

(define-public babashka
  (package
    (name "babashka")
    (version "1.12.194")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://github.com/babashka/babashka"
                                  "/releases/download/v" version "/babashka-"
                                  version "-linux-"
                                  (cond ((target-aarch64?)
                                         "aarch64")
                                        ((target-x86-64?)
                                         "amd64"))
                                  "-static.tar.gz"))
              (sha256
               (base32
                (cond ((target-aarch64?)
                       "0sxv0dqfvn98p6ki6m0awrcww3zgj5c1qx4s0sdphsj3pwg4w754")
                      ((target-x86-64?)
                       "0m0p3zq7m9zaj24rx7qm2sdxncg112yayy6l22dsxlxj6fpylf1b"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:patchelf-plan
      (if (target-aarch64?)
          `'(("bb" ("gcc" "libc" "zlib")))
          #f)
      #:install-plan
      `'(("./bb" "/bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chmod
            (lambda _
              (chmod "bb" #o755))))))
    (inputs (list `(,gcc "lib") zlib))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/babashka/babashka")
    (synopsis "Native, fast starting Clojure interpreter for scripting")
    (description "Babashka is a native Clojure interpreter for scripting with
fast startup.  Its main goal is to leverage Clojure in places where you would
be using bash otherwise.")
    (license license:epl1.0)))

(define-public clojure-lsp
  (package
    (name "clojure-lsp")
    (version "2024.08.05-18.16.00")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/clojure-lsp/clojure-lsp"
                                  "/releases/download/" version
                                  "/clojure-lsp-"
                                  (cond ((target-aarch64?)
                                         "aarch64")
                                        ((target-x86-64?)
                                         "native-linux-amd64"))
                                  ".zip"))
              (sha256
               (base32
                (cond ((target-aarch64?)
                       "0y9inzyw30vnp10r1kmm0f1gkrq0bc63vwiq7lqc1wqjqv1211r4")
                      ((target-x86-64?)
                       "1ixcisndcgr8i58rlf6ayvh6sjmvs4y1j1gnk4dhvssh7h66flhw"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan
      `'(("./clojure-lsp" "/bin/"))
      #:patchelf-plan
      (if (target-aarch64?)
          `'(("clojure-lsp" ("gcc" "libc" "zlib")))
          #f)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chmod
            (lambda _
              (chmod "./clojure-lsp" #o755))))))
    (inputs (list `(,gcc "lib") zlib))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/clojure-lsp/clojure-lsp")
    (synopsis "Clojure & ClojureScript Language Server (LSP) implementation")
    (description "This package provides a Language Server for Clojure and ClojureScript
languages.  The goal of this project is to bring great editing tools for
Clojure/Clojurescript to all editors and programatically via its CLI and API.
It aims to work alongside you to help you navigate, identify and fix errors,
perform refactors and more.")
    (license license:expat)))
