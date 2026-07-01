(define-module (r0man guix packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages zig)
  #:use-module (gnu packages)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix packages))

(define-public emacs-aider
  (package
    (name "emacs-aider")
    (version "0.13.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tninja/aider.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1932ghif057mxm9nz1213gf8j26ispl6ikfr90hrlag168c2f630"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-helm
                             emacs-magit
                             emacs-markdown-mode
                             emacs-s
                             emacs-transient))
    (home-page "https://github.com/tninja/aider.el")
    (synopsis "Emacs user interface for Aider")
    (description
     "This package provides an interactive interface to communicate with
Aider, an AI pair programming environment in your terminal.")
    (license license:asl2.0)))

(define-public emacs-aidermacs
  (package
    (name "emacs-aidermacs")
    (version "1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MatthewZMD/aidermacs.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11mmr222gk3g09rz4852yirxgrp0p7n7aa0x0hy7rfah7h2xy2vv"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-markdown-mode emacs-transient emacs-compat))
    (home-page "https://github.com/MatthewZMD/aidermacs")
    (synopsis "AI pair programming with Aider")
    (description
     "Aidermacs integrates with Aider (https://aider.chat/) for AI-assisted code
modification in Emacs.  Aider lets you pair program with LLMs to edit code in
your local git repository.  It works with both new projects and existing code
bases, supporting Claude, @code{DeepSeek}, @code{ChatGPT}, and can connect to
almost any LLM including local models.  Think of it as having a helpful coding
partner that can understand your code, suggest improvements, fix bugs, and even
write new code for you.  Whether you're working on a new feature, debugging, or
just need help understanding some code, Aidermacs provides an intuitive way to
collaborate with AI while staying in your familiar Emacs environment.
Originally forked from Kang Tu <tninja@@gmail.com>'s Aider.el.")
    (license license:asl2.0)))

(define-public emacs-avy-menu
  (let ((commit "bb694fd3dde6507f06f76dd862b888ba9c3b544d")
        (revision "1"))
    (package
      (name "emacs-avy-menu")
      (version (git-version "20230606.1519" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mrkkrp/avy-menu.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12ywbhba9sf4a4r49w2y63asgfi616krybzllc7h7b5ww62x7c42"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-avy))
      (home-page "https://github.com/mrkkrp/avy-menu")
      (synopsis "Library providing avy-powered popup menu")
      (description
       "The library provides an Avy-powered popup menu.  It is used in packages
such as @code{ace-popup-menu}, @code{char-menu}, and @code{hasky-extensions}.
It can also be used directly.")
      (license license:gpl3+))))

(define-public emacs-bnf-mode
  (let ((commit "1a7e177c282b8e07a2c33bd89232464b347dfc17")
        (revision "1"))
    (package
      (name "emacs-bnf-mode")
      (version (git-version "20221205.1451" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sergeyklay/bnf-mode.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1r23hrl258v7r0y785p2jrjz0y0bpd4lpl9ji91pqzrm6amvbkn4"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ; tests don't run
      (home-page "https://github.com/sergeyklay/bnf-mode")
      (synopsis "Major mode for editing BNF grammars.")
      (description
       "BNF Mode is a GNU Emacs major mode for editing BNF grammars.
Presently itprovides basic syntax and font-locking for BNF files.  BNF
notation is supportedexactly form as it was first announced in the
ALGOL 60 report.")
      (license license:gpl3+))))

(define-public emacs-cask
  (package
    (name "emacs-cask")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cask/cask.git")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1013cp97074ghjm3z7ah1xpgjwcr95pfibwg4lzvjj3nr8bcjnpp"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-s
           emacs-f
           emacs-epl
           emacs-shut-up
           emacs-package-build
           emacs-ansi))
    (arguments '(#:include '("^cask.el$" "^cask-bootstrap.el$")
                 #:exclude '()
                 #:tests? #f)) ; tests don't run
    (home-page "http://github.com/cask/cask")
    (synopsis "Cask: Project management for package development")
    (description
     "Cask is a project management tool for Emacs that helps automate the package
development cycle.")
    (license license:gpl3+)))

(define-public emacs-chatgpt
  (let ((commit "00e90708ce3e9e53f6f7739f2037d0dfb3b34d3e")
        (revision "3"))
    (package
      (name "emacs-chatgpt")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-openai/chatgpt")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02jxwydyg0jdp01lkxnphscl6i2a183wkhg8820s8b70v0svgcbw"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ; no tests
      (propagated-inputs (list emacs-ht
                               emacs-hydra
                               emacs-markdown-mode
                               emacs-openai
                               emacs-spinner))
      (home-page "https://github.com/emacs-openai/chatgpt")
      (synopsis "Use ChatGPT inside Emacs")
      (description "This Emacs Code extension allows you to use the official OpenAI API to
generate code or natural language responses from OpenAI's ChatGPT to
your questions, right within the editor.")
      (license license:gpl3+))))

(define-public emacs-clojure-mode-extra-font-locking
  (package
    (name "emacs-clojure-mode-extra-font-locking")
    (version "5.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure-emacs/clojure-mode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "123x8rwv4nb30h1rz7avshvr00xjfjjsmzrqsyxhgdm3f0rhac5w"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-clojure-mode))
    (arguments
     '(#:include '("^clojure-mode-extra-font-locking.el$")
       #:exclude '()
       #:tests? #f)) ; no tests
    (home-page "https://github.com/clojure-emacs/clojure-mode")
    (synopsis "Extra font-locking for Clojure mode")
    (description
     "This package provides additional font-locking for clojure-mode.  This
font-locking used to be part of clojure-mode up to version 3.0, but it was
removed due to its unreliable nature (the implementation is quite primitive and
font-locks symbols without any regard for what they resolve to).  CIDER provides
much more reliable font-locking, that's based on the runtime state of your
Clojure application.")
    (license license:gpl3+)))

(define-public emacs-clojure-ts-mode
  (package
    (name "emacs-clojure-ts-mode")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure-emacs/clojure-ts-mode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j78j9ig2x3g8qgsdrs38r3v0rva48c074d7kyag1aa0p7s37kr0"))))
    (build-system emacs-build-system)
    (arguments (list #:tests? #f)) ; tests don't run
    (home-page "https://github.com/clojure-emacs/clojure-ts-mode")
    (synopsis "Tree-sitter major mode for Clojure")
    (propagated-inputs
     (list tree-sitter-clojure))
    (description
     "Emacs major mode that provides font-lock (syntax highlighting),
indentation, and navigation support for the Clojure(Script)
programming language, powered by the tree-sitter-clojure tree-sitter
grammar.")
    (license license:gpl3+)))

(define-public emacs-codegpt
  (let ((commit "01602348999ec22ef93600e4f676b2cd63066dc9")
        (revision "4"))
    (package
      (name "emacs-codegpt")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-openai/codegpt")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kc7q4ipjx3h5wj0a7hpd582ivr1d013xr1q49qaz42rpvqglv38"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ; no tests
      (propagated-inputs (list emacs-chatgpt
                               emacs-markdown-mode
                               emacs-openai
                               emacs-spinner))
      (home-page "https://github.com/emacs-openai/codegpt")
      (synopsis "Use GPT-3 inside Emacs")
      (description "This Emacs Code extension allows you to use the official OpenAI API to
generate code or natural language responses from OpenAI's GPT-3 to
your questions, right within the editor.")
      (license license:gpl3+))))

(define-public emacs-consult-gh
  (package
    (name "emacs-consult-gh")
    (version "3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/armindarvish/consult-gh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p9lcjrzwxgscxpy0x992g8sxy6h3mcin1vffxf6nx28akwz13in"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-consult
                             emacs-embark
                             emacs-forge
                             emacs-markdown-mode
                             emacs-nerd-icons
                             emacs-ox-gfm
                             emacs-pr-review
                             emacs-yaml))
    (home-page "https://github.com/armindarvish/consult-gh")
    (synopsis "Interactive interface for GitHub CLI in Emacs using Consult")
    (description "Consult-GH provides an interface to interact with GitHub
repositories (search, view files and issues, clone, fork, etc) from
inside Emacs.  It uses the consult package and the GitHub CLI and
optionally Embark and provides an intuitive UI using minibuffer
completion familiar to Emacs users.")
    (license license:gpl3+)))

(define-public emacs-color-theme
  (package
    (name "emacs-color-theme")
    (version "20190220.1115")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacsattic/color-theme.git")
             (commit "3a2f6b615f5e2401e30d93a3e0adc210bbb4b7aa")))
       (sha256
        (base32 "09imx996afh3j207163kg8sc92hd7wljkpban1pnn6f67bgyrvlv"))))
    (build-system emacs-build-system)
    (arguments
     '(#:include
       '("^[^/]+.el$"
         "^[^/]+.el.in$"
         "^dir$"
         "^[^/]+.info$"
         "^[^/]+.texi$"
         "^[^/]+.texinfo$"
         "^doc/dir$"
         "^doc/[^/]+.info$"
         "^doc/[^/]+.texi$"
         "^doc/[^/]+.texinfo$"
         "^themes$")
       #:exclude
       '("^.dir-locals.el$"
         "^test.el$"
         "^tests.el$"
         "^[^/]+-test.el$"
         "^[^/]+-tests.el$")
       #:tests? #f)) ; tests don't run
    (home-page "http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme")
    (synopsis "An OBSOLETE color-theme implementation")
    (description
     "This package is obsolete.

Since version 22.1 Emacs has built-in support for themes.  That implementation
does not derive from the implementation provided by this package.  Back when
this was new we referred to the new implementation as @code{deftheme} themes, as
opposed to @code{color-theme} themes.

This package comes with a large collection of themes.  If you still use it
because you want to use one of those, then you can migrate to the new theme
implementation.  The @code{color-theme-modern} package ports all themes that are
bundled with @code{color-theme} to the @code{deftheme} format.  It also ports a
few third-party themes.")
    (license license:gpl3+)))

(define-public emacs-color-theme-solarized-r0man
  (package
    (inherit emacs-color-theme-solarized)
    (name "emacs-color-theme-solarized-r0man")
    (version "20210617")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/r0man/emacs-color-theme-solarized.git")
             (commit "3e250c48ea6b244c2c5af946d6cf6cfe24a6a4a5")))
       (sha256
        (base32 "1splha7sj2jcxlc8qh8d63b2531n36p74x722fnbf6fcfn3kkbs6"))))))

(define-public emacs-consult-recoll
  (package
    (name "emacs-consult-recoll")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/jao/consult-recoll")
                    (commit version)))
              (sha256
               (base32 "0w7c41fz6mm0i8annxr68icrcdmindafkvd3fnnnyw3ncm8vsygb"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-consult))
    (home-page "https://codeberg.org/jao/consult-recoll")
    (synopsis "Recoll queries in Emacs using consult")
    (description
     "Recoll is a local search engine that knows how to index a wide variety
of file formats, including PDFs, org and other text files and emails.
This package provides an emacs interface to perform recoll queries,
and display its results, via consult. It is also recommened that you
use a a package for vertical display of completions that works well
with consult, such as vertico.")
    (license license:gpl3+)))

(define-public emacs-copilot
  (let ((commit "6a2ad80489b8a0d021df95293eb7ac370aea140b")
        (revision "8"))
    (package
      (name "emacs-copilot")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/copilot-emacs/copilot.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1f2gxzg9vz6pwzaygqq99z5ii3ccpcv3031qnm03mql7zdwm29ba"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-dash emacs-f emacs-editorconfig emacs-jsonrpc emacs-s))
      (home-page "https://github.com/copilot-emacs/copilot.el")
      (synopsis "Unofficial Github Copilot mode for Emacs")
      (description "An unofficial Emacs mode for Github Copilot.")
      (license license:gpl3+))))

(define-public emacs-copilot-chat
  (package
    (name "emacs-copilot-chat")
    (version "20251211.0000")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chep/copilot-chat.el")
             (commit "ca446c226f08ae13fa6d173f4e3094a2e54adf09")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k9masn4937sx4j6sd5cw7fqi9q8v8n06i0ajsrk9c46zvs18afl"))))
    (build-system emacs-build-system)
    (arguments
     (list #:exclude #~(cons "^copilot-chat-mcp\\.el$" %default-exclude)))
    (propagated-inputs (list emacs-aio
                             emacs-magit
                             emacs-markdown-mode
                             emacs-mcp
                             emacs-org
                             emacs-polymode
                             emacs-request
                             emacs-shell-maker
                             emacs-transient))
    (home-page "https://github.com/chep/copilot-chat.el")
    (synopsis "Chat with Github copilot in Emacs")
    (description "This package allows you to chat with Github Copilot from within Emacs.")
    (license license:expat)))

(define-public emacs-efrit
  (let ((commit "97246b6e235a00cc827c3c9a3464bee168c1916b")
        (revision "6"))
    (package
      (name "emacs-efrit")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/steveyegge/efrit")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "181ljnv468va1d5p9yd9p572aqagl0s43khzb54fp7y9syfl8ch3"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:include
        #~(cons ".*\\.el$"
                '#$%default-include)
        #:lisp-directory "lisp"
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'set-home
              (lambda _
                (setenv "HOME" "/tmp")))
            (add-after 'expand-load-path 'add-subdirs-to-load-path
              (lambda _
                (let ((cwd (getcwd)))
                  (setenv "EMACSLOADPATH"
                          (string-append (string-join (cons cwd
                                                            (find-files cwd ""
                                                                        #:directories?
                                                                        #t))
                                                      ":") ":"
                                         (getenv "EMACSLOADPATH")))))))))
      (home-page "https://github.com/steveyegge/efrit")
      (synopsis "Native elisp coding agent running in Emacs")
      (description
       "A sophisticated AI coding agent that leverages Emacs' native
programmability through direct Elisp evaluation.")
      (license license:asl2.0))))

(define-public emacs-eglot-java
  (let ((commit "4cb3bdfaa954ad02e6eaac77c578987355db90cf")
        (revision "1"))
    (package
      (name "emacs-eglot-java")
      (version "20231228.2257")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yveszoundi/eglot-java")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 (base32
                         "0wsyd2yiz1air2aiwqvprz89qgm05ak8zksh1jg4m5w5gbg9rlrj"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-eglot emacs-jsonrpc))
      (home-page "https://github.com/yveszoundi/eglot-java")
      (synopsis "Eglot Java package for Emacs")
      (description "Java extension for the Eglot LSP client")
      (license license:gpl3+))))

(define-public emacs-elisa
  (package
    (name "emacs-elisa")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/s-kostyaev/elisa")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256 (base32
                       "0mk7rg2nmi7qc91fml0ahiwpc0xkhaf2w6mla8kxavjpx0051i3n"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-async emacs-ellama emacs-yaml))
    (home-page "https://github.com/s-kostyaev/elisa")
    (synopsis "Emacs Lisp Information System Assistant")
    (description
     "ELISA (Emacs Lisp Information System Assistant) is a project designed
to help Emacs users quickly find answers to their questions related to
Emacs and Emacs Lisp.  Utilizing the powerful Ellama package, ELISA
provides accurate and relevant responses to user queries, enhancing
productivity and efficiency in the Emacs environment.  By integrating
links to the Emacs info manual after answering a question, ELISA
ensures that users have easy access to additional information on the
topic, making it an essential tool for both beginners and advanced
Emacs users.")
    (license license:gpl3+)))

(define-public emacs-eval-expr
  (package
    (name "emacs-eval-expr")
    (version "20120619.647")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jwiegley/eval-expr.git")
             (commit "a0e69e83de41df8dbccefc1962ab4f02206a3328")))
       (sha256
        (base32 "08zw3qrhqmnv2wxmbf74svk2cx5by4831kyw6rx13imkc4x8kngx"))))
    (build-system emacs-build-system)
    (home-page "unspecified")
    (synopsis "enhanced eval-expression command")
    (description
     "This package provides an enhanced version of @code{eval-expression}.
It uses Emacs Lisp Mode syntax in the minibuffer, allows correcting
incomplete expressions without retyping, can display large results in a
separate buffer, and supports pretty-printing via @code{pp}.  The variables
@code{eval-expr-print-level} and @code{eval-expr-print-length} control
printing of recursive data structures independently of the global settings.")
    (license license:gpl3+)))

(define-public emacs-emacsql-sqlite
  (package
    (name "emacs-emacsql-sqlite")
    (version "4.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magit/emacsql")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10h0zb8pp4zbmsrkp8kd4a0rnclh6arbyg4f7n5l5n8cn0w2pf8l"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-emacsql))
    (arguments '(#:include '("^emacsql-sqlite.el$" "^sqlite$")
                 #:exclude '()
                 #:tests? #f))
    (home-page "https://github.com/magit/emacsql")
    (synopsis "EmacSQL back-end for SQLite")
    (description
     "During package installation EmacSQL will attempt to compile a custom native
binary for communicating with a SQLite database.")
    (license license:gpl3+)))

(define-public emacs-docopt
  (package
    (name "emacs-docopt")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/r0man/docopt.el.git")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1g3q6ymw5cxlm91zmwf8rilsz38rcgp2kki29ynk37vxc84bij13"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-dash emacs-f emacs-parsec emacs-s emacs-transient))
    (arguments
     `(#:include
       '("^[^/]+.el$"
         "^[^/]+.el.in$"
         "^dir$"
         "^[^/]+.info$"
         "^[^/]+.texi$"
         "^[^/]+.texinfo$"
         "^doc/dir$"
         "^doc/[^/]+.info$"
         "^doc/[^/]+.texi$"
         "^doc/[^/]+.texinfo$"
         "^src/[^/]+.el$")
       #:exclude
       '("^.dir-locals.el$"
         "^test.el$"
         "^tests.el$"
         "^[^/]+-test.el$"
         "^[^/]+-tests.el$")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'move-source-files
           (lambda _
             (let ((el-files (find-files "./src" ".*\\.el$")))
               (for-each (lambda (f) (rename-file f (basename f))) el-files)))))
       #:tests? #f)) ; tests require cask
    (home-page "https://github.com/r0man/docopt.el")
    (synopsis "A Docopt implementation in Elisp")
    (description "This package provides a Docopt implementation in Elisp")
    (license license:gpl3+)))

(define-public emacs-flycheck-elsa
  (let ((commit "d60db9544d0c4213f2478bcea0fd0e668e31cf34")
        (revision "1"))
    (package
      (name "emacs-flycheck-elsa")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-elsa/flycheck-elsa.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ig9dc7ch3cdxp4p24v21h7hrdph9y5jy9421bfww6agymbj8i85"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-flycheck))
      (home-page "https://github.com/emacs-elsa/flycheck-elsa")
      (synopsis "Flycheck for Elsa.")
      (description "Flycheck integration for Elsa.  See README.md")
      (license license:gpl3+))))

(define-public emacs-flymd
  (package
    (name "emacs-flymd")
    (version "20160617.1214")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mola-T/flymd.git")
             (commit "84d5a68bcfed4a295952c33ffcd11e880978d9d7")))
       (sha256
        (base32 "0j2mmr9f0d3zkhb92zc820iw4vkz958rm3ca7l9k3gx37cc4sn2l"))))
    (build-system emacs-build-system)
    (arguments
     '(#:include
       '("^[^/]+.el$"
         "^[^/]+.el.in$"
         "^dir$"
         "^[^/]+.info$"
         "^[^/]+.texi$"
         "^[^/]+.texinfo$"
         "^doc/dir$"
         "^doc/[^/]+.info$"
         "^doc/[^/]+.texi$"
         "^doc/[^/]+.texinfo$"
         "^flymd.html$")
       #:exclude
       '("^.dir-locals.el$"
         "^test.el$"
         "^tests.el$"
         "^[^/]+-test.el$"
         "^[^/]+-tests.el$")))
    (home-page "https://github.com/mola-T/flymd")
    (synopsis "On the fly markdown preview")
    (description
     "Flymd is an on-the-fly markdown preview package.  Open a markdown file,
run @code{M-x flymd-flyit}, and the file is opened in your browser.  When
finished, close the browser page and kill the markdown buffer.")
    (license license:gpl3+)))

(define-public emacs-ghostel
  (package
    (name "emacs-ghostel")
    (version "0.39.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dakra/ghostel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pik989dzzijmqkfqya8h188gk3n4jj5h5ikldzx046rfyrw7aw5"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Compile ghostel-module.so before entering lisp/ so the elisp
          ;; build sees the .so already next to ghostel.el.  Since 0.29
          ;; the module consumes ghostty-vt as a Zig *module* (@import),
          ;; not the libghostty-vt C ABI, so the whole VT engine is
          ;; compiled from source via upstream build.zig.  The build
          ;; sandbox has no network, so we pre-seed zig's global package
          ;; cache with the pinned ghostty checkout and every dependency
          ;; `zig build' resolves, then build fully offline.
          ;;
          ;; We populate $ZIG_GLOBAL_CACHE_DIR/p/<hash>/ and run plain
          ;; `zig build' rather than `zig build --system': the latter
          ;; forces a single-threaded path through the uucode comptime
          ;; Unicode-table generation that is roughly 15x slower.
          (add-after 'unpack 'build-native-module
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((zig (search-input-file inputs "/bin/zig"))
                    (cache (string-append (getcwd) "/.zig-global-cache")))
                (setenv "ZIG_GLOBAL_CACHE_DIR" cache)
                (setenv "ZIG_LOCAL_CACHE_DIR"
                        (string-append (getcwd) "/.zig-local-cache"))
                (for-each
                 (lambda (entry)
                   ;; ENTRY is (cache-dir-name source); a git-fetch
                   ;; source lowers to a directory, every other to a
                   ;; tarball to unpack with a single top-level strip.
                   (let* ((dst (car entry))
                          (src (cadr entry))
                          (dest (string-append cache "/p/" dst)))
                     (mkdir-p dest)
                     (if (file-is-directory? src)
                         (copy-recursively src dest)
                         (invoke "tar" "-xf" src "-C" dest
                                 "--strip-components=1"))))
                 `(;; The ghostty checkout pinned in ghostel's
                   ;; build.zig.zon; provides the ghostty-vt module and
                   ;; the vendored pkg/* C shims.
                   ("ghostty-1.3.2-dev-5UdBC7zDFAUYSL6EmAqDQxvWQCma-1lZ9QTXws90lLV6"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://github.com/ghostty-org/ghostty/archive/6246c288ae1087c8d67f75432a59da004b30bf25.tar.gz")
                       (sha256
                        (base32
                         "0zf82ziicl5ciyhgbj691vmgdgcwdnqsjbgj2czwmdjgvfc01cyz")))))
                   ;; uucode drives the comptime Unicode-table generation
                   ;; the ghostty-vt module needs.
                   ("uucode-0.2.0-ZZjBPqZVVABQepOqZHR7vV_NcaN-wats0IB6o-Exj6m9"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/uucode-0.2.0-ZZjBPqZVVABQepOqZHR7vV_NcaN-wats0IB6o-Exj6m9.tar.gz")
                       (sha256
                        (base32
                         "15az8qzp0rg5qj8ma0dam9j8jbf4wwb7wxsiq3iymmlb9w7yxayh")))))
                   ("uucode-0.1.0-ZZjBPj96QADXyt5sqwBJUnhaDYs_qBeeKijZvlRa0eqM"
                    (ungexp
                     (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/jacobsandlund/uucode")
                             (commit "5f05f8f83a75caea201f12cc8ea32a2d82ea9732")))
                       (file-name "uucode")
                       (sha256
                        (base32
                         "1zrdyhnqs0v46qasxb2kwd7694j8r8z6w4zlnfp42x8j6kwy2wxh")))))
                   ("N-V-__8AAGmZhABbsPJLfbqrh6JTHsXhY6qCaLAQyx25e0XE"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/highway-66486a10623fa0d72fe91260f96c892e41aceb06.tar.gz")
                       (sha256
                        (base32
                         "04m21b46h6c4x099r9qb720ql9llpzz8yq3k94i8zq7l7s4zim47")))))
                   ;; The remaining entries are ghostty's other build.zig
                   ;; dependencies.  ghostel builds only the ghostty-vt
                   ;; module, but ghostty's build() constructs its full
                   ;; graph, so every dependency it references must be in
                   ;; the cache (a plain `zig build' would otherwise try
                   ;; to fetch the missing ones over the network).
                   ("zigimg-0.1.0-8_eo2vHnEwCIVW34Q14Ec-xUlzIoVg86-7FU2ypPtxms"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://github.com/ivanstepanovftw/zigimg/archive/d7b7ab0ba0899643831ef042bd73289510b39906.tar.gz")
                       (sha256
                        (base32
                         "0ly53dd3pj8hl3kkf3h8x4dw79yb7riwj9qc9da18mdkl9mxf7ic")))))
                   ("vaxis-0.5.1-BWNV_LosCQAGmCCNOLljCIw6j6-yt53tji6n6rwJ2BhS"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/vaxis-7dbb9fd3122e4ffad262dd7c151d80d863b68558.tar.gz")
                       (sha256
                        (base32
                         "1xlf12dlzda0z4d3svq0qibvfgqzkrv4igg6qqg58nwwr0mk6wif")))))
                   ("z2d-0.10.0-j5P_Hu-6FgBsZNgwphIqh17jDnj8_yPtD8yzjO6PpHRQ"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/z2d-0.10.0-j5P_Hu-6FgBsZNgwphIqh17jDnj8_yPtD8yzjO6PpHRQ.tar.gz")
                       (sha256
                        (base32
                         "1xwpcw2awxf2r1kz27m0j4pzpi5g92gd1i2mzqvhkvnmxyi1vwk9")))))
                   ("zf-0.10.3-OIRy8RuJAACKA3Lohoumrt85nRbHwbpMcUaLES8vxDnh"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/zf-3c52637b7e937c5ae61fd679717da3e276765b23.tar.gz")
                       (sha256
                        (base32
                         "0s25gjvp7rns1l52jvgbd7aakndlvfs5xh9b4wk9wkphia95s09v")))))
                   ("libxev-0.0.0-86vtc4IcEwCqEYxEYoN_3KXmc6A9VLcm22aVImfvecYs"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/libxev-34fa50878aec6e5fa8f532867001ab3c36fae23e.tar.gz")
                       (sha256
                        (base32
                         "1mvx91wn7499xfx76fxijq4x66x1g5yk4cpr52hii9g4jrmyl0v0")))))
                   ("gobject-0.3.0-Skun7ANLnwDvEfIpVmohcppXgOvg_I6YOJFmPIsKfXk-"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/gobject-2025-11-08-23-1.tar.zst")
                       (sha256
                        (base32
                         "0j0csvsyvp0193mpkdp25s14kargppmdyslbhi5qw788y0347gfr")))))
                   ("N-V-__8AAAzZywE3s51XfsLbP9eyEw57ae9swYB9aGB6fCMs"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/wuffs-122037b39d577ec2db3fd7b2130e7b69ef6cc1807d68607a7c232c958315d381b5cd.tar.gz")
                       (sha256
                        (base32
                         "04qwpr8c4xjla4skwb1fpvkjc0c611qhbhz9xp3c9rlnpq5d4k4y")))))
                   ("N-V-__8AAB0eQwD-0MdOEBmz7intriBReIsIDNlukNVoNu6o"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/zlib-1220fed0c74e1019b3ee29edae2051788b080cd96e90d56836eea857b0b966742efb.tar.gz")
                       (sha256
                        (base32
                         "0p6h2i9ajdp46lckdpibfqy4vz5nh5r22bqq96mp41k0ydiqis0p")))))
                   ("N-V-__8AABzkUgISeKGgXAzgtutgJsZc0-kkeqBBscJgMkvy"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/glslang-12201278a1a05c0ce0b6eb6026c65cd3e9247aa041b1c260324bf29cee559dd23ba1.tar.gz")
                       (sha256
                        (base32
                         "1dcpm70fhxk07vk37f5l0hb9gxfv6pjgbqskk8dfbcwwa2xyv8hl")))))
                   ("N-V-__8AADYiAAB_80AWnH1AxXC0tql9thT-R-DYO1gBqTLc"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/pixels-12207ff340169c7d40c570b4b6a97db614fe47e0d83b5801a932dcd44917424c8806.tar.gz")
                       (sha256
                        (base32
                         "06pi3f3lhyxfzczhwrc2b4n0jhhzydbz96qlpw12a24is0b3ps2m")))))
                   ("N-V-__8AAEbOfQBnvcFcCX2W5z7tDaN8vaNZGamEQtNOe0UI"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://github.com/ocornut/imgui/archive/refs/tags/v1.92.5-docking.tar.gz")
                       (sha256
                        (base32
                         "1jzr65gpx4mqfcdbnf2rm2kd20jmj9whwdb7x1df3wvmih7c45n8")))))
                   ("N-V-__8AAG02ugUcWec-Ndp-i7JTsJ0dgF8nnJRUInkGLG7G"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/harfbuzz-11.0.0.tar.xz")
                       (sha256
                        (base32
                         "16rb7aazy36pj3xrjy149dd90j9yv7q5jnqx5kz2air1zsx52qzi")))))
                   ("N-V-__8AAG3RoQEyRC2Vw7Qoro5SYBf62IHn3HjqtNVY6aWK"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/libxml2-2.11.5.tar.gz")
                       (sha256
                        (base32
                         "05b2kbccbkb5pkizwx2s170lcqvaj7iqjr5injsl5sry5sg0aa3c")))))
                   ("N-V-__8AAGi9AwC7QV7hLqjN6iBkXA2y5dxw285nkSLlVB7I"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/ghostty-themes-release-20260525-155808-7335c0a.tgz")
                       (sha256
                        (base32
                         "0ifyjw1xxap54h1j49di0mpnapwd4020mc3igx4danhxsi2s6sqw")))))
                   ("N-V-__8AAHjwMQDBXnLq3Q2QhaivE0kE2aD138vtX2Bq1g7c"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/oniguruma-1220c15e72eadd0d9085a8af134904d9a0f5dfcbed5f606ad60edc60ebeccd9706bb.tar.gz")
                       (sha256
                        (base32
                         "187jk4fxdkzc0wrcx4kdy4v6p1snwmv8r97i1d68yi3q5qha26h0")))))
                   ("N-V-__8AAIC5lwAVPJJzxnCAahSvZTIlG-HhtOvnM1uh-66x"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/JetBrainsMono-2.304.tar.gz")
                       (sha256
                        (base32
                         "1i2w213919avi0apgbw720wqy0z46a89bwv3b65hkbc2icg6jyn5")))))
                   ("N-V-__8AAIrfdwARSa-zMmxWwFuwpXf1T3asIN7s5jqi9c1v"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/fontconfig-2.14.2.tar.gz")
                       (sha256
                        (base32
                         "0mcarq6v9k7k9a8is23vq9as0niv0hbagwdabknaq6472n9dv8iv")))))
                   ("N-V-__8AAJrvXQCqAT8Mg9o_tk6m0yf5Fz-gCNEOKLyTSerD"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/libpng-1220aa013f0c83da3fb64ea6d327f9173fa008d10e28bc9349eac3463457723b1c66.tar.gz")
                       (sha256
                        (base32
                         "0fm0y7543w2gx5sz3zg9i46x1am51c77a554r0zqwpphdjs9bk7y")))))
                   ("N-V-__8AAKLKpwC4H27Ps_0iL3bPkQb-z6ZVSrB-x_3EEkub"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/freetype-1220b81f6ecfb3fd222f76cf9106fecfa6554ab07ec7fdc4124b9bb063ae2adf969d.tar.gz")
                       (sha256
                        (base32
                         "035r5bypzapa1x7za7lpvpkz58fxynz4anqzbk8705hmspsh2wj2")))))
                   ("N-V-__8AAMVLTABmYkLqhZPLXnMl-KyN38R8UVYqGrxqO26s"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/NerdFontsSymbolsOnly-3.4.0.tar.gz")
                       (sha256
                        (base32
                         "010d7gkv359qg555d89i4hhgb56c8f69kw5jsx4f5gflaswx2r0i")))))
                   ("N-V-__8AANb6pwD7O1WG6L5nvD_rNMvnSc9Cpg1ijSlTYywv"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/spirv_cross-1220fb3b5586e8be67bc3feb34cbe749cf42a60d628d2953632c2f8141302748c8da.tar.gz")
                       (sha256
                        (base32
                         "1qspcsx56v0mddarb6f05i748wsl2ln3d8863ydsczsyqk7nyaxm")))))
                   ("N-V-__8AANT61wB--nJ95Gj_ctmzAtcjloZ__hRqNw5lC1Kr"
                    (ungexp
                     (origin
                       (method url-fetch)
                       (uri "https://deps.files.ghostty.org/DearBindings_v0.17_ImGui_v1.92.5-docking.tar.gz")
                       (sha256
                        (base32
                         "18xsf0zisr9q8xqa8g3kh4qkqb9rrikkvqiyhmwzc9h9w00cbzlb")))))))
                ;; Emits ghostel-module.so and its sidecar
                ;; ghostel-module.version (the version the elisp loader
                ;; compares against to detect a stale module) at the
                ;; source root.  Drop both next to ghostel.el so the
                ;; elisp byte-compile can load the module and the
                ;; install phase can pick them up.
                (invoke zig "build" "-Doptimize=ReleaseFast" "-Dcpu=baseline")
                (copy-file "ghostel-module.so" "lisp/ghostel-module.so")
                (copy-file "ghostel-module.version"
                           "lisp/ghostel-module.version"))))
          (add-after 'build-native-module 'enter-lisp-dir
            (lambda _
              ;; Keep LICENSE reachable from the build dir so
              ;; `install-license-files' picks it up.
              (copy-file "LICENSE" "lisp/LICENSE")
              (chdir "lisp")))
          ;; Several .el files contain the literal sequence `"/bin/sh"'
          ;; (a runtime shell literal, not a build path), which trips
          ;; `patch-el-files' while it scans for /bin/<exec> references.
          ;; No patching is needed here.
          (delete 'patch-el-files)
          (add-after 'install 'install-native-module
            (lambda _
              ;; emacs-build-system's `install' phase only copies
              ;; .el/.elc/.info/.texi, so drop the rest next to the
              ;; installed ghostel.el (the MELPA-flat layout that
              ;; `ghostel--resource-root' probes for first):
              ;;   - ghostel-module.so, the native module;
              ;;   - ghostel-module.version, the sidecar the elisp
              ;;     loader reads to detect a stale on-disk module
              ;;     without `module-load'-ing it first;
              ;;   - etc/, holding shell integration
              ;;     (etc/shell/ghostel.{bash,zsh,fish} + bootstrap/)
              ;;     and bundled terminfo (etc/terminfo/{x,78}/xterm-ghostty).
              ;; Still inside `lisp/' from `enter-lisp-dir', so the
              ;; source `etc/' tree sits one level up.
              (let ((dest (car (find-files (string-append #$output
                                            "/share/emacs/site-lisp")
                                           "^ghostel-[0-9]"
                                           #:directories? #t))))
                (install-file "ghostel-module.so" dest)
                (install-file "ghostel-module.version" dest)
                (copy-recursively "../etc"
                                  (string-append dest "/etc"))))))))
    (native-inputs (list zig-0.15 zstd))
    ;; 0.29+ compiles the ghostty-vt engine (uucode comptime Unicode
    ;; tables) from source.  On aarch64 that comptime pass is
    ;; single-threaded and slow enough to blow past the build daemon's
    ;; --max-silent-time; only x86_64 is verified to build.
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/dakra/ghostel")
    (synopsis "Emacs terminal emulator using libghostty-vt")
    (description
     "Ghostel is an Emacs terminal emulator powered by libghostty-vt, the
same VT engine that drives the Ghostty terminal.  Inspired by emacs-libvterm,
a native dynamic module handles terminal state and rendering, while Elisp
manages the shell process, keymap, and buffer.  This package compiles the
native module (including the ghostty-vt engine) from source, so @code{M-x
ghostel} works out-of-the-box with no download or compile prompt.")
    (license license:gpl3+)))

(define-public emacs-github-browse-file
  (package
    (name "emacs-github-browse-file")
    (version "20160205.1427")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/osener/github-browse-file.git")
             (commit "9742a5183af853788c6ecb83fb7ee0b00d1675ac")))
       (sha256
        (base32 "0i3dkm0j4gh21b7r5vxr6dddql5rj7lg8xlaairvild0ccf3bhdl"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/osener/github-browse-file")
    (synopsis "View the file you're editing on GitHub")
    (description
     "Call @code{github-browse-file} (for the git blob) or
@code{github-browse-file-blame} (for the git blame) to view the current file
on GitHub.  With a prefix argument (C-u), you can force them to use the
@code{master} branch.")
    (license license:gpl3+)))

(define-public emacs-guess-language
  (let ((commit "bccca49bbb27a3dff02c660d7cda8cdf83333d6e")
        (revision "1"))
    (package
      (name "emacs-guess-language")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tmalsburg/guess-language.el.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0hq3nsjwvjxjrpqv57q8krw59gdd14qmkpm4pii49ca8iyx65v54"))))
      (build-system emacs-build-system)
      (arguments
       '(#:include '("^guess-language.el$" "^trigrams/[^/]+$") #:exclude '()))
      (home-page "https://github.com/tmalsburg/guess-language.el")
      (synopsis "Robust automatic language detection")
      (description
       "Guess-language is a buffer-local minor mode.  It guesses the language of the
current paragraph when flyspell detects an incorrect word and changes Ispell's
dictionary and typo-mode (if present) accordingly.  If the language settings
change, flyspell is rerun but only on the current paragraph.  Guess-language
thus supports documents using multiple languages.

If the paragraph is shorter than some user-defined value, none of the above
happens because there is likely not enough text to guess the language correctly.

Custom functions can be triggered when a new language is detected such that
users can do things like changing the input method when needed.

The detection algorithm is based on counts of character trigrams.  At this time,
supported languages are Arabic, Czech, Danish, Dutch, English, Esperanto,
Finnish, French, German, Italian, Norwegian, Polish, Portuguese, Russian,
Serbian, Slovak, Slovenian, Spanish, Swedish and Vietnamese.  Adding further
languages is very easy and this package already contains language statistics for
49 additional languages.")
      (license license:gpl3+))))

(define-public emacs-grip-mode
  (package
    (name "emacs-grip-mode")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/seagle0128/grip-mode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mrpmzcxgc9hkv55d6kv79f8y6ldqrf5b48f0a4fy30qlgby2wyz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/seagle0128/grip-mode")
    (synopsis "Instant GitHub-flavored Markdown/Org preview using grip")
    (description "Instant GitHub-flavored Markdown/Org preview using a grip subprocess.")
    (license license:gpl3+)))

(define-public emacs-jira
  (package
    (name "emacs-jira")
    (version "2.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unmonoqueteclea/jira.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wwzw84qrzzw1bywpfz81x4z9fb84dikxgb5dx4hji97j1s6w21k"))))
    (build-system emacs-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (propagated-inputs (list emacs-magit
                             emacs-request
                             emacs-tablist
                             emacs-transient))
    (home-page "https://github.com/unmonoqueteclea/jira.el")
    (synopsis "Emacs Interface to Jira")
    (description "This package allows you to visualuze and manipulate Jira
issues from Emacs.")
    (license license:gpl3+)))

(define-public emacs-jiralib2
  (package
    (name "emacs-jiralib2")
    (version "20200520.2031")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nyyManni/jiralib2")
             (commit "c21c4e759eff549dbda11099f2f680b78d7f5a01")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yrcc9yfz9gxkhizy03bpysl1wcdbk0m6fj9hkqw3kbgnsk25h4p"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-request emacs-dash))
    (home-page "https://github.com/nyyManni/jiralib2")
    (synopsis "JIRA REST API bindings to Elisp")
    (description
     "This package provides a programmatic interface to JIRA via its REST API.
It provides access to JIRA from other programs, but no user level
functionality.  It supports three methods of authentication: cookie, basic,
and token.  OAuth login is not supported.")
    (license license:gpl3+)))

(define-public emacs-json-process-client
  (let ((commit "6485953fe6eff62938fd08720811c6fdd09d7d22")
        (revision "2"))
    (package
      (name "emacs-json-process-client")
      (version (git-version "0.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url
                       "https://github.com/DamienCassou/json-process-client")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 (base32
                         "1vqls817hp6x7ydqyn1k7akj2pwdzi7iwp995zas24yk5w04dd5i"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ; tests use networking
      (home-page "https://github.com/DamienCassou/json-process-client")
      (synopsis "Interact with a TCP process using JSON")
      (description
       "This library starts a process and communicates with it through JSON
over TCP.  The process must output one JSON message per line.")
      (license license:gpl3+))))

(define-public emacs-kubel
  (let ((commit "e0138bf95583fd525e4d4cc17c4bc5ba884c3af9")
        (revision "2"))
    (package
      (name "emacs-kubel")
      (version (git-version "3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/abrochard/kubel")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1nlrj9nfgdz8vs10cpfb9maqczvn7jg8nmj8c41dzgc0wn3z8l7r"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-transient emacs-dash emacs-s emacs-yaml-mode))
      (arguments
       '(#:include
         '("^[^/]+.el$"
           "^[^/]+.el.in$"
           "^dir$"
           "^[^/]+.info$"
           "^[^/]+.texi$"
           "^[^/]+.texinfo$"
           "^doc/dir$"
           "^doc/[^/]+.info$"
           "^doc/[^/]+.texi$"
           "^doc/[^/]+.texinfo$")
         #:exclude
         '("^.dir-locals.el$"
           "^test.el$"
           "^tests.el$"
           "^[^/]+-test.el$"
           "^[^/]+-tests.el$"
           "^kubel-evil.el$")))
      (home-page "https://github.com/abrochard/kubel")
      (synopsis "Control Kubernetes with limited permissions")
      (description
       "Emacs extension for controlling Kubernetes with limited permissions.")
      (license license:gpl3+))))

(define-public emacs-language-detection
  (package
    (name "emacs-language-detection")
    (version "20161123.1813")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andreasjansson/language-detection.el")
             (commit "54a6ecf55304fba7d215ef38a4ec96daff2f35a4")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p8kim8idh7hg9398kpgjawkxq9hb6fraxpamdkflg8gjk0h5ppa"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/andreasjansson/language-detection.el")
    (synopsis "Automatic language detection from code snippets")
    (description
     "Automatic programming language detection using a pre-trained random forest
classifier.  It supports many languages including Ada, AWK, C, Clojure, C++,
C#, CSS, Dart, Delphi, Emacs Lisp, Erlang, Go, Haskell, HTML, Java,
JavaScript, JSON, LaTeX, Lisp, Lua, Perl, PHP, Python, R, Ruby, Rust, Scala,
Shell, SQL, Swift, and XML among others.  The main entry points are
@code{language-detection-buffer} and @code{language-detection-string}.")
    (license license:gpl3+)))

(define-public emacs-lsp-dart
  (package
    (name "emacs-lsp-dart")
    (version "1.24.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-lsp/lsp-dart")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0spc9wmqllb4kcn4cvyj2f7k6lzaz2gd86msf49raqddf023665f"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-lsp-treemacs
           emacs-lsp-mode
           emacs-dap-mode
           emacs-f
           emacs-dash
           emacs-dart-mode))
    (home-page "https://emacs-lsp.github.io/lsp-dart/")
    (synopsis "Dart support lsp-mode")
    (description "Dart analysis server client for LSP mode.")
    (license license:gpl3+)))

(define-public emacs-markdown-preview-eww
  (package
    (name "emacs-markdown-preview-eww")
    (version "20160111.1502")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/niku/markdown-preview-eww")
             (commit "5853f836425c877c8a956501f0adda137ef1d3b7")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i5gr3j9dq41p2zl4bfyvzv6i5z7hgrxzrycmbdc3s7nja36k9z4"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/niku/markdown-preview-eww")
    (synopsis "Realtime preview by eww")
    (description "This package provides the realtime markdown preview by eww.")
    (license license:gpl3+)))

(define-public emacs-mcp
  (let ((commit "125e0a4478ff1404880ea4e593f5e4ff0122cb83")
        (revision "3"))
    (package
      (name "emacs-mcp")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lizqwerscott/mcp.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02rapj0vgnml518hr1vxvv7bljpa6ldcnxi676gysng2p1kn4f0q"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/lizqwerscott/mcp.el")
      (synopsis "Emacs MCP client")
      (description "MCP.el is an Emacs client for connecting to @acronym{MCP, Model
Context Protocol} servers.  It supports structured communication with
both filesystem and generic MCP servers, offers asynchronous and
synchronous operations, and provides resource management features.
The package includes an extensible tool and prompt system, server
lifecycle controls, and integrates with popular Emacs packages such as
gptel and llm.")
      (license license:gpl3+))))

(define-public emacs-mcp-server-lib
  (package
    (name "emacs-mcp-server-lib")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/laurynas-biveinis/mcp-server-lib.el")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qn38iqf75xzz0vkgac2ngv49qv52rbbfpziczb052r5fnl0pzp0"))))
    (build-system emacs-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (home-page "https://github.com/laurynas-biveinis/mcp-server-lib.el")
    (synopsis "Model Context Protocol server library for Emacs Lisp")
    (description
     "This library enables Emacs packages to expose their functionality to AI
applications via the Model Context Protocol (MCP).  It provides infrastructure
for Emacs packages to expose their functionality as tools and resources to
Large Language Models.  Features include a simple API for registering tools
and resources, handles MCP protocol communication and JSON-RPC messages,
stdio transport via emacsclient wrapper script, and built-in usage metrics
and debugging support.")
    (license license:gpl3+)))

(define-public emacs-mermaid-mode
  (let ((commit "9535d513b41ed11bcd91f644815e2db6430c1560")
        (revision "1"))
    (package
      (name "emacs-mermaid-mode")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/abrochard/mermaid-mode")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "174xlsm316rpxm8sbxfnyhvy1bjkl0qbx71s1mfcyda737y3dsbl"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/abrochard/mermaid-mode")
      (synopsis "Major mode for working with mermaid graphs")
      (description
       "Major mode for working with mermaid graphs.
See @url{https://mermaid-js.github.io/}.")
      (license license:gpl3+))))

(define-public emacs-tblui
  (let ((commit "62ab5f62982c061a902fd3e54d94a68a4706572c")
        (revision "1"))
    (package
      (name "emacs-tblui")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Yuki-Inoue/tblui.el")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1fq0dsvh9a8h7n4d4cj7sn73nzbg5chqjby9pzpbs5grx4kf0zi6"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-dash emacs-magit-popup emacs-tablist))
      (home-page "https://github.com/Yuki-Inoue/tblui.el")
      (synopsis "Define tabulated list mode based UIs more easily")
      (description "This package can be used to define user interfaces that are similar to
Emacs built-in tabulated list mode, but with less boilerplate.")
      (license license:gpl3+))))

(define-public emacs-ob-mermaid
  (let ((commit "372c2d91d3cdba5da9f7ac23e7bce7a0b3b46862")
        (revision "1"))
    (package
      (name "emacs-ob-mermaid")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arnm/ob-mermaid")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1bvg7xgm9ph7hbkgzm145ifxi833rg6lamanngqq564n7d0l4ng0"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/arnm/ob-mermaid")
      (synopsis "Org-babel support for Mermaid diagrams")
      (description "Generate Mermaid diagrams using org-mode, org-babel and the Mermaid
CLI.")
      (license license:gpl3+))))

(define-public emacs-ox-tufte
  (package
    (name "emacs-ox-tufte")
    (version "4.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ox-tufte/ox-tufte")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256 (base32
                       "0jn0f5r3yy0kcrdspkpk7hhblfd41qf8ayi7z207albvkvyisl4i"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-org))
    (arguments '(#:tests? #f
                 #:include '("^[^/]+.el$" "^[^/]+.el.in$"
                             "^dir$"
                             "^[^/]+.info$"
                             "^[^/]+.texi$"
                             "^[^/]+.texinfo$"
                             "^doc/dir$"
                             "^doc/[^/]+.info$"
                             "^doc/[^/]+.texi$"
                             "^doc/[^/]+.texinfo$"
                             "^src$")
                 #:exclude '("^.dir-locals.el$" "^test.el$" "^tests.el$"
                             "^[^/]+-test.el$" "^[^/]+-tests.el$")))
    (home-page "https://github.com/ox-tufte/ox-tufte")
    (synopsis "Emacs Org-mode export backend for Tufte HTML")
    (description "This package provides an export backend for Org mode that exports
buffers to HTML that is compatible with Tufte CSS.")
    (license license:gpl3+)))

(define-public emacs-openai
  (let ((commit "349aaccee567fceda116a05a310b401e320f186b")
        (revision "3"))
    (package
      (name "emacs-openai")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-openai/openai")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0n4ymfv0hl2h9rarqw6ybf1lkjpr1mcgvw46d1i2rh11ldphymfx"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ; no tests
      (propagated-inputs (list emacs-dash emacs-request emacs-tblui))
      (home-page "https://github.com/emacs-openai/openai")
      (synopsis "Elisp library for the OpenAI API")
      (description "The OpenAI Elisp library provides convenient access to the OpenAI API
from applications written in the Elisp language.")
      (license license:gpl3+))))

(define-public emacs-ox-slack
  (package
    (name "emacs-ox-slack")
    (version "20200108.1546")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/titaniumbones/ox-slack")
                    (commit "bd797dcc58851d5051dc3516c317706967a44721")))
              (file-name (git-file-name name version))
              (sha256 (base32
                       "1kh2v08fqmsmfj44ik8pljs3fz47fg9zf6q4mr99c0m5ccj5ck7w"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-org emacs-ox-gfm))
    (home-page "https://github.com/titaniumbones/ox-slack")
    (synopsis "Slack Exporter for org-mode")
    (description
     "This library implements a Slack backend for the Org exporter, based on the
@code{md} and @code{gfm} back-ends.")
    (license license:gpl3+)))

(define-public emacs-dall-e
  (let ((commit "036941cf7f39226d9cdd86ca331941c69c16f2ca")
        (revision "2"))
    (package
      (name "emacs-dall-e")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-openai/dall-e")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "028cnyih81x1qxa92z8a5xf1kzngpg34775pjk8c3grh5lr0q0cv"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ; no tests
      (propagated-inputs (list emacs-async
                               emacs-ht
                               emacs-hydra
                               emacs-openai
                               emacs-reveal-in-folder
                               emacs-spinner))
      (home-page "https://github.com/emacs-openai/dall-e")
      (synopsis "Use DALL-E inside Emacs")
      (description "This Emacs Code extension allows you to use the official OpenAI API to
generate generate digital images from natural language descriptions.")
      (license license:gpl3+))))

(define-public emacs-reveal-in-folder
  (let ((commit "70c9ba29705ce6a0fc7a718cc4d6016b9691d408")
        (revision "2"))
    (package
      (name "emacs-reveal-in-folder")
      (version (git-version "0.1.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jcs-elpa/reveal-in-folder")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "160a4j2n89m73si7h5r9ljxmx657v3g643y4f1a1iww7kw24kbg6"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ; no tests
      (propagated-inputs (list emacs-f emacs-s))
      (home-page "https://github.com/jcs-elpa/reveal-in-folder")
      (synopsis "Reveal current file in folder")
      (description "Reveal current file in folder.")
      (license license:gpl3+))))

(define-public emacs-org-gcal
  (let ((commit "c954d3b1a8f7a23ec5c4410e56dfa7b08a45f769")
        (revision "1"))
    (package
      (name "emacs-org-gcal")
      (version (git-version "0.4.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kidd/org-gcal.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ji4xgnbnn85xaw4wpg3r37nh7ncihl36xl87ag7l3vhbw1bwzcm"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ; no tests
      (propagated-inputs
       (list emacs-aio
             emacs-dash
             emacs-request
             emacs-request-deferred
             emacs-alert
             emacs-persist
             emacs-org))
      (home-page "https://github.com/kidd/org-gcal.el")
      (synopsis "Org sync with Google Calendar")
      (description "Org sync with Google Calendar")
      (license license:gpl3+))))

(define-public emacs-ox-jira
  (let ((commit "8748a908ee366e52539b19b863935d2ef7ebd0bf")
        (revision "2"))
    (package
      (name "emacs-ox-jira")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stig/ox-jira.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0b2cirdyxz7c26d999kyzi7lydxh23v925bf2jmfv229875c8izv"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-org))
      (home-page "https://github.com/stig/ox-jira.el")
      (synopsis "Org-mode export backend for JIRA markup")
      (description
       "This module plugs into the regular Org Export Engine and transforms
Org files to JIRA markup for pasting into JIRA tickets & comments.")
      (license license:gpl3+))))

(define-public emacs-request-deferred
  (let ((commit "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6")
        (revision "2"))
    (package
      (name "emacs-request-deferred")
      (version (git-version "0.3.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tkf/emacs-request")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "15affk5cp6va3d8wf8567l45nri4ayiwk52p7i40h7nafjq4wp04"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-deferred emacs-request))
      (arguments '(#:include '("^request-deferred.el$")
                   #:exclude '()
                   #:tests? #f))
      (home-page "https://github.com/tkf/emacs-request")
      (synopsis "Wrap request.el by deferred")
      (description
       "Trivial wrapper to request library returing kiwanami deferred object.")
      (license license:gpl3+))))

(define-public emacs-scss-mode
  (let ((commit "cf58dbec5394280503eb5502938f3b5445d1b53d")
        (revision "1"))
    (package
      (name "emacs-scss-mode")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/antonj/scss-mode")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0raja19l0igwr0pn0ghr1pj1d8i9k3m3764ma4r8nwzxcj9qw4ja"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/antonj/scss-mode")
      (synopsis "Major mode for editing SCSS files")
      (description
       "Command line utility sass is required, see @url{http://sass-lang.com/}.
To install sass: @code{gem install sass}.  Also make sure sass location is
in Emacs PATH, or customize @code{scss-sass-command} to point to your
sass executable.")
      (license license:gpl3+))))

(define-public emacs-smooth-scrolling
  (let ((commit "7b65c4304f97e186c290ce2c7cd2922eaae692e0")
        (revision "1"))
    (package
      (name "emacs-smooth-scrolling")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aspiers/smooth-scrolling")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0fcq8p84ykk3alqnv28phbczibg8fgkc8rs9ff14qcwjv0kgak9r"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/aspiers/smooth-scrolling")
      (synopsis "Make emacs scroll smoothly")
      (description
       "This package offers a global minor mode which make Emacs scroll
smoothly.  It keeps the point away from the top and bottom of the current
buffer's window in order to keep lines of context around the point visible
as much as possible, whilst minimising the frequency of sudden scroll jumps
which are visually confusing.")
      (license license:gpl3+))))

(define-public emacs-timesheet
  (let ((commit "511751b239c84d7619ec1c61d7f108b732b64442")
        (revision "1"))
    (package
      (name "emacs-timesheet")
      (version (git-version "0.5.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tmarble/timesheet.el.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "01j8wps938mjjlq55w9lgky3f51k260ipjqv8072x0n73mwf9008"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-s emacs-org emacs-auctex))
      (arguments '(#:include '("^[^/]+.el$" "^bin$" "^share$")
                   #:exclude '()
                   #:tests? #f))
      (home-page "https://github.com/tmarble/timesheet.el")
      (synopsis "Timesheet management add-on for org-mode")
      (description "Timesheet management add-on for Emacs org-mode")
      (license license:gpl3+))))

(define-public emacs-paimon
  (package
    (name "emacs-paimon")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/r0man/paimon.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x3xqgmkba6hqsdbp71wmd9fx6lji40y1kprpg8jxxvax1m4vpi3"))))
    (build-system emacs-build-system)
    (native-inputs
     (list sqlite))
    (propagated-inputs
     (list emacs-aio
           emacs-closql
           emacs-emacsql
           emacs-f
           emacs-ht
           emacs-transient
           emacs-request))
    (arguments
     `(#:include
       '("^[^/]+.el$"
         "^[^/]+.el.in$"
         "^dir$"
         "^[^/]+.info$"
         "^[^/]+.texi$"
         "^[^/]+.texinfo$"
         "^doc/dir$"
         "^doc/[^/]+.info$"
         "^doc/[^/]+.texi$"
         "^doc/[^/]+.texinfo$"
         "^src/[^/]+.el$")
       #:exclude
       '("^.dir-locals.el$"
         "^test.el$"
         "^tests.el$"
         "^[^/]+-test.el$"
         "^[^/]+-tests.el$")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'move-source-files
           (lambda _
             (let ((el-files (find-files "./src" ".*\\.el$")))
               (for-each (lambda (f) (rename-file f (basename f))) el-files)))))
       #:tests? #f)) ; no tests
    (home-page "https://github.com/r0man/paimon.el")
    (synopsis "Major mode for Splunk")
    (description "This package provides a major mode for Splunk.")
    (license license:gpl3+)))

(define-public emacs-sayid
  (package
    (name "emacs-sayid")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/clojure-emacs/sayid")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vdz3dxwi02an5k956apq3ah0dpzly9zd44fhmrqlcjimxc69m7p"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'enter-lisp-directory
            (lambda _
              (chdir "src/el"))))
      #:tests? #f)) ; no tests
    (propagated-inputs (list emacs-cider))
    (home-page "https://github.com/clojure-emacs/sayid")
    (synopsis "Sayid Emacs mode")
    (description
     "Sayid (siy EED) is an omniscient debugger and profiler for Clojure.
It intercepts and records the inputs and outputs of user selected
functions.  This package is the Emacs front-end to it, which allows
the user to select functions to trace, and display the recorded inputs
and outputs from within Emacs.")
    (license license:asl2.0)))

(define-public emacs-semext
  (let ((commit "6d05e243d066c2f8b3cd44081ea31cb1c445e535")
        (revision "0"))
    (package
      (name "emacs-semext")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ahyatt/semext")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0w4hwy25zfwbkjgg1s8n2ns2xapn4ba9miykp9l75q0lic0pa4al"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/ahyatt/semext")
      (propagated-inputs (list emacs-llm))
      (synopsis "Semantic versions of existing Emacs functionality")
      (description "Semext aims to provide LLM-powered version of Emacs command in ways
that feel natural to Emacs users.  The goal is to be as Emacs-like as
possible.")
      (license license:gpl3+))))

(define-public emacs-sqlite3
  (let ((commit "8509f05938cfc946ad1d3927ce1c3b88f8500281")
        (revision "1"))
    (package
      (name "emacs-sqlite3")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/syohex/emacs-sqlite3")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1cwg0m7lf8dhgf8484a9p8r0gyk9891759m52mkwvidbkyrcwjzp"))))
      (build-system emacs-build-system)
      (arguments
       `(#:modules ((guix build emacs-build-system)
                    (guix build emacs-utils)
                    (guix build utils))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-module-load
             (lambda* (#:key outputs #:allow-other-keys)
               (chmod "sqlite3.el" #o644)
               (emacs-substitute-sexps "sqlite3.el"
                 ("(require 'cl-lib)"
                  `(unless (require 'sqlite3-core nil t)
                     (module-load ,(string-append (assoc-ref outputs "out") "/lib/sqlite3-core.so")))))))
           (add-before 'install 'build-emacs-module
             ;; Run make.
             (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
               ;; Compile the shared object file.
               (apply invoke "make" "all" make-flags)
               ;; Move the file into /lib.
               (install-file "sqlite3-core.so"
                             (string-append (assoc-ref outputs "out") "/lib")))))
         #:tests? #f))
      (native-inputs
       (list libtool sqlite))
      (home-page "https://github.com/syohex/emacs-sqlite3")
      (synopsis "Sqlite3 binding of Emacs Lisp")
      (description "Sqlite binding of Emacs Lisp inspired by mruby-sqlite3")
      (license license:gpl3+))))

(define-public emacs-sqlite3-api
  (let ((commit "615cc1cb021384291cec39a2048a3a36859e8cb9")
        (revision "1"))
    (package
      (name "emacs-sqlite3-api")
      (version (git-version "0.18" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pekingduck/emacs-sqlite3-api")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0lkzd3s08iymh1ahh3bfs6h70wafh5avxkx1rymq3facc9nc87jp"))))
      (build-system emacs-build-system)
      (arguments
       `(#:modules ((guix build emacs-build-system)
                    (guix build emacs-utils)
                    (guix build utils))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-module-load
             (lambda* (#:key outputs #:allow-other-keys)
               (chmod "sqlite3.el" #o644)
               (emacs-substitute-sexps "sqlite3.el"
                 ("(require 'sqlite3-api nil t)"
                  `(module-load ,(string-append
                                  (assoc-ref outputs "out")
                                  "/lib/sqlite3-api.so"))))))
           (add-before 'install 'build-emacs-module
             ;; Run make.
             (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
               ;; Compile the shared object file.
               (apply invoke "make" "CC=gcc" make-flags)
               ;; Move the file into /lib.
               (install-file "sqlite3-api.so"
                             (string-append (assoc-ref outputs "out") "/lib")))))
         #:tests? #f))
      (native-inputs
       (list libtool sqlite))
      (home-page "https://github.com/pekingduck/emacs-sqlite3-api")
      (synopsis "SQLite3 API for GNU Emacs 25+")
      (description "SQLite3 is a dynamic module for GNU Emacs 25+ that provides
direct access to the core SQLite3 C API from Emacs Lisp.")
      (license license:gpl3+))))

(define-public emacs-virtualenvwrapper
  (package
    (name "emacs-virtualenvwrapper")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/porterjamesj/virtualenvwrapper.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "062pbnplb3w9h64qsj71d9fvgicp1x63n05mgvgymjh2rnx7py0d"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash emacs-s))
    (home-page "https://github.com/porterjamesj/virtualenvwrapper.el")
    (synopsis "Featureful virtualenv tool for Emacs")
    (description
     "This package provides a featureful virtualenv tool for Emacs.
It emulates much of the functionality of Doug Hellmann's virtualenvwrapper.")
    (license license:gpl3+)))

(define-public emacs-whisper
  (let ((commit "6198ce3d9bff0555cf098a77b78d6c2d79baf4f9")
        (revision "2"))
    (package
      (name "emacs-whisper")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/natrys/whisper.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0f72sa92hz0nxq469ajgwjnriwqbqq6snwxqhrzz0izhwmnkmks5"))))
      (build-system emacs-build-system)
      (inputs (list whisper-cpp))
      (home-page "https://github.com/natrys/whisper.el")
      (synopsis "Speech-to-Text interface for Emacs using OpenAI's whisper model")
      (description "Speech-to-Text interface for Emacs using OpenAI’s whisper speech
recognition model.  For the inference engine it uses the C/C++ port
whisper.cpp that can run on consumer grade CPU without requiring a
high end GPU.")
      (license license:gpl3+))))

(define-public emacs-wsd-mode
  (package
    (name "emacs-wsd-mode")
    (version "20191031.1211")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/josteink/wsd-mode.git")
             (commit "53330a2a43c4875f8682457df1a869a4c9028660")))
       (sha256
        (base32 "1swwaphlbvpalk7mgdgkaal8ffivhxil5fhfkxzazqnkl4i5zbmy"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/josteink/wsd-mode")
    (synopsis "Emacs major-mode for www.websequencediagrams.com")
    (description
     "This is a major mode for modelling and editing sequence diagrams using the
syntax employed by @url{https://www.websequencediagrams.com}.  It supports
inline rendering of diagrams through the website API, saving diagrams to
image files, syntax highlighting, automatic indentation of block statements,
and WSD premium features such as SVG export when an API key is provided.")
    (license license:gpl3+)))

(define-public emacs-pgemacs
  (package
    (name "emacs-pgemacs")
    (version "0.42")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emarsden/pgmacs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pf8y64hka1fl47dphmh4xgxiwfsd0g4q2fazq5yc48zwr9nsf02"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/emarsden/pgmacs")
    (propagated-inputs (list emacs-pg))
    (synopsis "Emacs editing PostgreSQL databases")
    (description "PGmacs provides an editing interface for the PostgreSQL
object-relational DBMS from Emacs.")
    (license license:gpl3+)))

(define-public emacs-editor-code-assistant
  (package
    (name "emacs-editor-code-assistant")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/editor-code-assistant/eca-emacs")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "053qqrwlrnaa306kq8fz9s2dlaavgd71frxikkyq7ph6gnj2p6wf"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/editor-code-assistant/eca-emacs")
    (propagated-inputs (list emacs-compat emacs-dash emacs-f
                             emacs-markdown-mode emacs-s))
    (synopsis "Editor Code Assistant for Emacs")
    (description "Editor Code Assistant (ECA) integration for Emacs.")
    (license license:asl2.0)))

(define-public emacs-claudemacs
  (let ((commit "5ba3416fc9e61575870b53424981cab00d1de825")
        (revision "2"))
    (package
      (name "emacs-claudemacs")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cpoile/claudemacs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03cqa499c8za8k0lxgjbkgl4mya9bv2fv4wq2mqxvnq7k5q35pyn"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ; no tests
      (home-page "https://github.com/cpoile/claudemacs")
      (synopsis "AI Pair Programming with Claude Code in Emacs")
      (description "AI Pair Programming with Claude Code in Emacs")
      (license license:expat))))

(define-public emacs-claude-code
  (let ((commit "4a9914bd4161eb43f489820f9174c62390e5adc8")
        (revision "4"))
    (package
      (name "emacs-claude-code")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stevemolitor/claude-code.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "13pbbypq5xjmp8wpiin9n6hiylsz85gpfj9dmwjffwb1mpm46a91"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ; no tests
      (home-page "https://github.com/stevemolitor/claude-code.el")
      (propagated-inputs (list emacs-eat emacs-inheritenv emacs-transient emacs-vterm))
      (synopsis "Claude Code Emacs integration")
      (description "Emacs interface for Claude Code CLI, providing integration between
Emacs and Claude AI for coding assistance.")
      (license license:asl2.0))))

(define-public emacs-claude-code-ide
  (let ((commit "a9485f766ea69f6cb3a3f08dea20d44fd6596673")
        (revision "1"))
    (package
      (name "emacs-claude-code-ide")
      (version (git-version "0.2.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/manzaltu/claude-code-ide.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1mr1pjjllgnfsdyc4ilhj0fgyavjmax6ga90qrqwf2l2ily96ipa"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ; no tests
      (home-page "https://github.com/manzaltu/claude-code-ide.el")
      (propagated-inputs
       (list emacs-transient
             emacs-web-server
             emacs-websocket))
      (synopsis "Claude Code IDE integration for Emacs")
      (description "Claude Code IDE for Emacs provides seamless integration with Claude
Code CLI through the Model Context Protocol (MCP).  This package
enables AI-powered code assistance directly within your Emacs
workflow.")
      (license license:gpl3+))))

(define-public emacs-clipetty
  (package
    (name "emacs-clipetty")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spudlyo/clipetty")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k3srxvy5r7hbwbr8r65l9gc7nynqrqx5hc39s3xgx3ddq66wq4i"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/spudlyo/clipetty")
    (synopsis "Send Emacs kill-ring to system clipboard in TTY")
    (description
     "Clipetty enables clipboard sharing when using Emacs in a terminal (TTY)
environment.  It sends text that you kill in Emacs to your operating system's
clipboard using OSC 52 escape sequences.  This allows seamless clipboard
integration between terminal-based Emacs and other applications, even when
running over SSH or inside terminal multiplexers like tmux or GNU Screen.")
    (license license:gpl3+)))

(define-public emacs-shell-maker
  (package
    (name "emacs-shell-maker")
    (version "0.90.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xenodium/shell-maker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06ibxnb765sjcxjv2w923cnjr9664qp2pf7w3wxdp8vrbxg1s670"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/xenodium/shell-maker")
    (synopsis "Emacs package for creating interactive command shells")
    (description
     "Shell-maker provides a framework for creating interactive command shells
in Emacs.  It offers a foundation for building shell-like interfaces that can
communicate with various backends and services, supporting features like
command history, auto-completion, and customizable prompts.")
    (license license:gpl3+)))

(define-public emacs-acp
  (package
    (name "emacs-acp")
    (version "0.11.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xenodium/acp.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hr1176sy8xrx6wkqadmvwdjm1sv7aq8ddrw8h3ha6sn74glx8ws"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/xenodium/acp.el")
    (synopsis "Emacs client library for the Agent Client Protocol")
    (description
     "ACP.el is an Emacs Lisp implementation of the Agent Client Protocol,
providing a standardized interface for communicating with AI agents and
language models.  It enables Emacs to interact with various AI services
through a unified protocol.")
    (license license:gpl3+)))

(define-public emacs-agent-shell
  (package
    (name "emacs-agent-shell")
    (version "0.50.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xenodium/agent-shell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0njajpz51pbz4hqaq7lcvwaypilq1c9sdxsk6sdxgk1xpivqlxfb"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-shell-maker emacs-acp))
    (home-page "https://github.com/xenodium/agent-shell")
    (synopsis "Native Emacs shell for interacting with LLM agents via ACP")
    (description
     "Agent-shell provides a native Emacs shell interface for interacting with
Large Language Model (LLM) agents through the Agent Client Protocol (ACP).
It enables users to chat with AI agents like Claude, Gemini, or OpenAI's models
directly within Emacs, supporting features like tool calls, file operations,
and interactive command execution.  The package integrates seamlessly with
Emacs workflows, allowing AI agents to read and write files, execute commands,
and assist with development tasks.")
    (license license:gpl3+)))

(define-public emacs-mcp-client
  (let ((commit "11585f6702168ea11e85880f565f5733d17852e0")
        (revision "2"))
    (package
      (name "emacs-mcp-client")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/r0man/emacs-mcp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "086gn3p2sv3xzp0pykh7h404iswps6qipd012rlvszsk55an45ak"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/r0man/emacs-mcp")
      (synopsis "Model Context Protocol client for Emacs")
      (description "This package provides a Model Context Protocol (MCP) client
implementation for Emacs, enabling integration with AI tools and
services that support the MCP standard.")
      (license license:gpl3+))))

(define-public emacs-kele
  (package
    (name "emacs-kele")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jinnovation/kele.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1df9b4ifav19b8724fpp35hpynkjadqxi5x7m0nj3kfyi46fl831"))))
    (build-system emacs-build-system)
    (arguments (list #:tests? #f)) ; no tests
    (propagated-inputs
     (list emacs-async
           emacs-dash
           emacs-f
           emacs-magit
           emacs-memoize
           emacs-plz
           emacs-yaml))
    (home-page "https://github.com/jinnovation/kele.el")
    (synopsis "Kubernetes management for Emacs")
    (description
     "Kele (Kubernetes Enablement Layer for Emacs) provides a nimble,
lightweight interface for managing Kubernetes clusters from within Emacs.
It enables fast, efficient Kubernetes operations without requiring
context-switching from your editor.

Key features include:
@itemize
@item Enhanced resource viewing with a sortable, color-coded tabular
interface that improves upon @code{kubectl get}
@item Context-aware actions that present only relevant operations for each
resource type
@item Menu bar controls for switching Kubernetes contexts and namespaces
@item Integration with doom-modeline to display the active kubectl context
and default namespace in the modeline
@item Integration with Embark for additional contextual actions
@end itemize

The package follows a batteries-included philosophy, coming pre-configured
with useful integrations while remaining highly customizable.")
    (license license:asl2.0)))

(define-public emacs-podman
  (let ((commit "93f19860badedb0ad1519358fda441940ef688e7")
        (revision "1"))
    (package
      (name "emacs-podman")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/akirak/podman.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1vr7m1jikr0j9hy6qlmvcd3xckqw33v0cqmd1p3gdng7q1xkr2h1"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-tablist emacs-transient emacs-ts))
      (home-page "https://github.com/akirak/podman.el")
      (synopsis "Manage Podman pods from Emacs")
      (description
       "This package provides an Emacs interface for managing Podman pods.
It displays pods in a tabular interface via the @code{podman-pod-list} command
and allows performing operations on pods interactively.  It complements
@code{docker.el} by providing Podman-specific pod management features.")
      (license license:gpl3+))))

(define-public emacs-madolt
  (let ((commit "84df9091faefe1c7b0b919d438e82156b75f58cd")
        (revision "3"))
    (package
      (name "emacs-madolt")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aspiers/madolt")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xw20z9762kqkdl0zpkagwwnnlb1xq6hb69nqgs2h5dkxpvqiy6g"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:tests? #f))
      (propagated-inputs (list emacs-compat emacs-magit emacs-transient
                               emacs-with-editor))
      (home-page "https://github.com/aspiers/madolt")
      (synopsis "Magit-like interface for Dolt databases")
      (description
       "Madolt provides a magit-like Emacs interface for Dolt, the
version-controlled SQL database.  It offers a section-based, keyboard-driven UI
for Dolt's Git-like version control operations on SQL databases.  Features
include viewing status, staging tables, committing changes, diffing at the row
and cell level, branching, merging, rebasing, cherry-picking, stashing, blame,
conflict resolution, remote operations, and running arbitrary SQL queries.")
      (license license:gpl3+))))
