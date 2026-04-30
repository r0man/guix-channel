(define-module (r0man guix packages clojure)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages readline)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nonguix build-system binary)
  #:use-module (nongnu packages clojure))

(define-public babashka
  (package
    (name "babashka")
    (version "1.12.214")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://github.com/babashka/babashka"
                           "/releases/download/v"
                           version
                           "/babashka-"
                           version
                           "-linux-"
                           (cond
                             ((target-aarch64?)
                              "aarch64")
                             ((target-x86-64?)
                              "amd64"))
                           "-static.tar.gz"))
       (sha256
        (base32 (cond
                  ((target-aarch64?)
                   "1zj5j6gr5lhympl2bjrdhjf72im0259mdgfd56s07cqwlhl6vg5n")
                  ((target-x86-64?)
                   "18xlvbj78pasaf9vcn8rcjgd5022hi3ip1q722r31qpn023hj9i9"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:patchelf-plan (if (target-aarch64?)
                          `'(("bb" ("gcc" "libc" "zlib"))) #f)
      #:install-plan `'(("./bb" "/bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chmod
            (lambda _
              (chmod "bb" #o755))))))
    (inputs (list (list gcc "lib") zlib))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/babashka/babashka")
    (synopsis "Native, fast starting Clojure interpreter for scripting")
    (description
     "Babashka is a native Clojure interpreter for scripting with
fast startup.  Its main goal is to leverage Clojure in places where you would
be using bash otherwise.")
    (license license:epl1.0)))

(define-public clojure-lsp
  (package
    (name "clojure-lsp")
    (version "2025.11.28-12.47.43")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/clojure-lsp/clojure-lsp"
                           "/releases/download/"
                           version
                           "/clojure-lsp-native-linux-"
                           (cond
                             ((target-aarch64?)
                              "aarch64")
                             ((target-x86-64?)
                              "amd64"))
                           ".zip"))
       (sha256
        (base32 (cond
                  ((target-aarch64?)
                   "0gy1va0bkrkhffbpl99m1x306ivyhj4jm586sxw3vha5qqb0k61i")
                  ((target-x86-64?)
                   "03fdmzf2jg0p4xfq1db26c4p1k12g318ra5fxh519314127kf8b0"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan `'(("./clojure-lsp" "/bin/"))
      #:patchelf-plan (if (target-aarch64?)
                          `'(("clojure-lsp" ("gcc" "libc" "zlib"))) #f)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chmod
            (lambda _
              (chmod "./clojure-lsp" #o755))))))
    (inputs (list (list gcc "lib") zlib))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/clojure-lsp/clojure-lsp")
    (synopsis "Clojure & ClojureScript Language Server (LSP) implementation")
    (description
     "This package provides a Language Server for Clojure and ClojureScript
languages.  The goal of this project is to bring great editing tools for
Clojure/Clojurescript to all editors and programatically via its CLI and API.
It aims to work alongside you to help you navigate, identify and fix errors,
perform refactors and more.")
    (license license:expat)))


(define-public editor-code-assistant
  (package
    (name "editor-code-assistant")
    (version "0.92.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/editor-code-assistant"
                           "/eca/releases/download/"
                           version
                           "/eca-native-linux-"
                           (cond
                             ((target-aarch64?)
                              "aarch64")
                             ((target-x86-64?)
                              "amd64"))
                           ".zip"))
       (sha256
        (base32 (cond
                  ((target-aarch64?)
                   "1sqz90rxysp4p2zf7kvdkhggxn7p65hn3zy5njx3x5j75srs083g")
                  ((target-x86-64?)
                   "0divw47yficwcxfj9sn2hkgwhbwjsm1s4qqkngz6ch52icnqikvz"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:patchelf-plan (if (target-aarch64?)
                          `'(("eca" ("gcc" "libc" "zlib"))) #f)
      #:install-plan `'(("eca" "/bin/"))))
    (inputs (list `(,gcc "lib") zlib))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/editor-code-assistant/eca")
    (synopsis "Editor Code Assistant")
    (description "AI pair programming capabilities in any editor.")
    (license license:expat)))

(define-public clojure-tools-bin-latest
  (package
    (inherit clojure-tools-bin)
    (name "clojure-tools-bin-latest")
    (version "1.12.4.1582")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.clojure.org/install/clojure-tools-"
             version ".tar.gz"))
       (sha256
        (base32 "08gzfblnz0zhnk6pwr9vcm6y168psgrwmqww3wqk1v7j5gr68n7x"))))))

(define-public bbin
  (package
    (name "bbin")
    (version "0.2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/babashka/bbin")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12x5r0m1slbzh9xblq90yqqparijj31ghn88jj9qa8lhcbjbsicj"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("bbin" "bin/")
          ("templates" "share/bbin/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (bbin (string-append bin "/bbin"))
                     (bb (search-input-file inputs "/bin/bb"))
                     (templates (string-append out "/share/bbin/templates")))
                (wrap-program bbin
                  `("PATH" ":" prefix (,bin))
                  `("BBIN_TEMPLATES" ":" = (,templates)))
                (substitute* bbin
                  (("#!/usr/bin/env bb")
                   (string-append "#!" bb)))))))))
    (inputs (list bash-minimal babashka))
    (home-page "https://github.com/babashka/bbin")
    (synopsis "Install Babashka scripts and projects")
    (description
     "bbin is a package manager for Babashka scripts and projects.
It allows you to install any Babashka script or project with a single
command from various sources including qualified library names, URLs,
Git repositories, local files, and local directories.  Once installed,
scripts become executable commands.")
    (license license:expat)))
