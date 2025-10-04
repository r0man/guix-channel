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
    (version "1.12.208")
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
                       "16cxzqf62jxavn5dizfn0isgxjfgy538in3f7xz7xv5q144kqqpz")
                      ((target-x86-64?)
                       "0abivcyg16cmv6phd1xcs8f971p34v0ibqaic7qyqcirpvl0nk22"))))))
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
    (version "2025.08.25-14.21.46")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/clojure-lsp/clojure-lsp"
                                  "/releases/download/" version
                                  "/clojure-lsp-native-linux-"
                                  (cond ((target-aarch64?)
                                         "aarch64")
                                        ((target-x86-64?)
                                         "amd64"))
                                  ".zip"))
              (sha256
               (base32
                (cond ((target-aarch64?)
                       "1k8a269hyhdzxhxb6kcz6snn3wz8bygki29fy4xvww8r72xa1yl5")
                      ((target-x86-64?)
                       "07vmnrzagylp2yd95hg0dpflg2j24z7m6bvv8pry52p49323vncf"))))))
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

(define-public editor-code-assistant
  (package
    (name "editor-code-assistant")
    (version "0.64.0")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/editor-code-assistant"
                                  "/eca/releases/download/"
                                  version "/eca-native-linux-"
                                  (cond ((target-aarch64?)
                                         "aarch64")
                                        ((target-x86-64?)
                                         "amd64"))
                                  ".zip"))
              (sha256
               (base32
                (cond ((target-aarch64?)
                       "0imvb35gwg1zb6kh4b144kg039p06n9ysvsavfdzby45hq13r8lf")
                      ((target-x86-64?)
                       "0f8y3bd7lvidbdl8d50g8fxpnyvm6pfc3sk0naqxli0yws5j2crc"))))))
    (build-system binary-build-system)
    (arguments (list #:install-plan `'(("eca" "/bin/"))))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/editor-code-assistant/eca")
    (synopsis "Editor Code Assistant")
    (description "AI pair programming capabilities in any editor")
    (license license:expat)))
