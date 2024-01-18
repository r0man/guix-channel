(define-module (r0man guix packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public emacs-avy-menu
  (package
    (name "emacs-avy-menu")
    (version "20210321.1732")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mrkkrp/avy-menu.git")
             (commit "18bb320f395b7e412f7e377cf4c46d205d4b4e1a")))
       (sha256
        (base32 "0p96kxyq1pk9cnwxis80vr6xnklcg2n8gp7pj8z41kz1i2fn960m"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-avy))
    (home-page "https://github.com/mrkkrp/avy-menu")
    (synopsis "Library providing avy-powered popup menu")
    (description
     "The library provides an Avy-powered popup menu.  It is used in (at least) the
following packages:

* `ace-popup-menu' * `char-menu' * `hasky-extensions'

It can also be used directly.")
    (license #f)))

(define-public emacs-bnf-mode
  (package
    (name "emacs-bnf-mode")
    (version "20200323.1348")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sergeyklay/bnf-mode.git")
                    (commit "d9329dd90e5d4f629295e85898362d9682047898")))
              (sha256
               (base32
                "11ka3qb3wz6bs9b9bc6kwyxcxzrsmq9s8292lyzd0kknd162qx2z"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/sergeyklay/bnf-mode")
    (synopsis "Major mode for editing BNF grammars.")
    (description
     "BNF Mode is a GNU Emacs major mode for editing BNF grammars.
Presently itprovides basic syntax and font-locking for BNF files.  BNF
notation is supportedexactly form as it was first announced in the
ALGOL 60 report.")
    (license #f)))

(define-public emacs-cask
  (package
    (name "emacs-cask")
    (version "20211103.1654")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cask/cask.git")
             (commit "751435bd56c7123d8244d9d156309e21e63cd5c0")))
       (sha256
        (base32 "0jxwsaam7hqvqbxrkzd7hy09l87yjsg7z89jikzs6vzc72h02g8x"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-s
           emacs-f
           emacs-epl
           emacs-shut-up
           emacs-package-build
           emacs-ansi))
    (arguments '(#:include '("^cask.el$" "^cask-bootstrap.el$") #:exclude '()))
    (home-page "http://github.com/cask/cask")
    (synopsis "Cask: Project management for package development")
    (description
     "Cask is a project management tool for Emacs that helps automate the package
development cycle.")
    (license #f)))

(define-public emacs-clojure-mode-extra-font-locking
  (package
    (name "emacs-clojure-mode-extra-font-locking")
    (version "20211230.817")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure-emacs/clojure-mode.git")
             (commit "e31186843d06ea86f3771244d1cde0112f9e2079")))
       (sha256
        (base32 "0dlbwz0vkn2sf394r86s7vbc78jkq7wd3ldziqkwf57ci2068nyi"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-clojure-mode))
    (arguments
     '(#:include '("^clojure-mode-extra-font-locking.el$") #:exclude '()))
    (home-page "http://github.com/clojure-emacs/clojure-mode")
    (synopsis "Extra font-locking for Clojure mode")
    (description
     "This package provides additional font-locking for clojure-mode.  This
font-locking used to be part of clojure-mode up to version 3.0, but it was
removed due to its unreliable nature (the implementation is quite primitive and
font-locks symbols without any regard for what they resolve to).  CIDER provides
much more reliable font-locking, that's based on the runtime state of your
Clojure application.")
    (license #f)))

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
         "^[^/]+-tests.el$")))
    (home-page "http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme")
    (synopsis "An OBSOLETE color-theme implementation")
    (description
     "This package is obsolete.

Since version 22.1 Emacs has built-in support for themes.  That implementation
does not derive from the implementation provided by this package.  Back when
this was new we referred to the new implementation as `deftheme' themes, as
opposed to `color-theme' themes.

This package comes with a large collection of themes.  If you still use it
because you want to use one of those, then you can never-the-less migrate to the
\"new\" theme implementation.  The `color-theme-modern' package ports all themes
that are bundles with `color-theme' to the `deftheme' format.  It also ports a
few third-party themes.  Its documentation contains setup instructions.  Don't
forget to uninstall `color-theme'.

; Thanks

Deepak Goel  <deego@glue.umd.edu> S.  Pokrovsky <pok@nbsp.nsk.su> for ideas and
discussion.  Gordon Messmer <gordon@dragonsdawn.net> for ideas and discussion.
Sriram Karra <karra@cs.utah.edu> for the color-theme-submit stuff.  Olgierd
`Kingsajz' Ziolko <kingsajz@rpg.pl> for the spec-filter idea.  Brian Palmer for
color-theme-library ideas and code All the users that contributed their color
themes.

\f")
    (license #f)))

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
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://elpa.gnu.org/packages/consult-recoll-" version
                    ".tar"))
              (sha256 (base32
                       "02vg1rr2fkcqrrivqgggdjdq0ywvlyzazwq1xd02yah3j4sbv4ag"))))
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
  (let ((commit "8f5e45405ead77fcbe85b5c02193f23449d2d518"))
    (package
      (name "emacs-copilot")
      (version (git-version "0.0.1" "3" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/copilot-emacs/copilot.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0g2iqndi97m9y6mk3hj6nn4a1zvaivxqrbbv0nwdvxhxpywx9kdp"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'patch-base-dir
              (lambda _
                (substitute* (find-files "." "\\.el")
                  (("\\(or load-file-name")
                   (string-append "(or \"" #$output "/share/emacs/\"")))))
            (add-after 'install 'install-dist
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((dist (string-append #$output "/share/emacs/dist")))
                  (mkdir-p dist)
                  (copy-recursively "dist" dist)))))))
      (propagated-inputs (list emacs-dash emacs-editorconfig emacs-jsonrpc emacs-s))
      (home-page "https://github.com/zerolfx/copilot.el")
      (synopsis "Unofficial Github Copilot mode for Emacs")
      (description "An unofficial Emacs mode for Github Copilot.")
      (license license:gpl3+))))

(define-public emacs-eldoc-box
  (package
    (name "emacs-eldoc-box")
    (version "20230228.237")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/casouri/eldoc-box.git")
                    (commit "16fbf1f17f09a8308d5e5df3a3a97277baa5736a")))
              (sha256
               (base32
                "100dlzzwaf09i3vp0dj8maf1is0pnp3r21iix41ymxrfcafx6hi1"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/casouri/eldoc-box")
    (synopsis "Display ElDoc documentation in childframe")
    (description "This package displays ElDoc documentations in a childframe.")
    (license license:gpl3+)))

(define-public emacs-ellama
  (package
    (name "emacs-ellama")
    (version "0.5.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/s-kostyaev/ellama.git")
                    (commit "0.5.7")))
              (sha256 (base32
                       "16rwqard3mi4i27q4wmpbgy9mb1irg21vfqx2f5j0g1g44wqnzik"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-llm emacs-spinner))
    (home-page "http://github.com/s-kostyaev/ellama")
    (synopsis "Tool for interacting with LLMs")
    (description
     "Ellama is a tool for interacting with large language models from Emacs.  It
allows you to ask questions and receive responses from the
LLMs. Ellama can perform various tasks such as translation, code
review, summarization, enhancing grammar/spelling or wording and more
through the Emacs interface.  Ellama natively supports streaming
output, making it effortless to use with your preferred text editor.")
    (license #f)))

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
     "Updates of this program may be available via the URL
http://www.splode.com/~friedman/software/emacs-lisp/

To use this package, put this in your .emacs:

   (require 'eval-expr)    (eval-expr-install)

Highlights:

  * When reading the Lisp object interactively from the minibuffer, the
minibuffer uses the Emacs Lisp Mode syntax table.  (Emacs 19.18 or     later
only.)

  * If you type an incomplete or otherwise syntactically invalid     expression
(e.g.  you forget a closing paren), you can fix your     mistake without having
to type it all over again.

  * Can display the result in a buffer if it is too big to fit in the     echo
area.  This buffer is placed in Emacs Lisp Mode.      (If you give a prefix arg,
the result is placed in the current     buffer instead of the echo area or a
separate temporary buffer.)

  * The variables `eval-expr-print-level' and `eval-expr-print-length'     can
be used to constrain the attempt to print recursive data     structures.  These
variables are independent of the global     `print-level' and `print-length'
variables so that eval-expression     can be used more easily for debugging.

  * Pretty-printing complex results via `pp' function is possible.

This program is loosely based on an earlier implemention written by Joe Wells
<jbw@cs.bu.edu> called eval-expression-fix.el (last revised 1991-10-12).  That
version was originally written for Emacs 18 and, while it worked with some
quirky side effects in Emacs 19, created even more problems in Emacs 20 and
didn't work in XEmacs at all.

This rewrite should work in Emacs 19.18 or later and any version of XEmacs.
However it will not work in Emacs 18.")
    (license #f)))

(define-public emacs-emacsql-sqlite
  (package
    (name "emacs-emacsql-sqlite")
    (version "20220218.1543")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skeeto/emacsql.git")
             (commit "374726060d74df0e2bcb9d0355ff41e2c400ed30")))
       (sha256
        (base32 "0z382qksrwhkv0ayjp8nays65c3xwd4kylj41k1pc3nnqg6b2k45"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-emacsql))
    (arguments '(#:include '("^emacsql-sqlite.el$" "^sqlite$") #:exclude '()))
    (home-page "https://github.com/skeeto/emacsql")
    (synopsis "EmacSQL back-end for SQLite")
    (description
     "During package installation EmacSQL will attempt to compile a custom native
binary for communicating with a SQLite database.")
    (license #f)))

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
               (for-each (lambda (f) (rename-file f (basename f))) el-files)))))))
    (home-page "https://github.com/r0man/docopt.el")
    (synopsis "A Docopt implementation in Elisp")
    (description "This package provides a Docopt implementation in Elisp")
    (license #f)))

(define-public emacs-flycheck-clj-kondo
  (package
    (name "emacs-flycheck-clj-kondo")
    (version "20211227.2226")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/borkdude/flycheck-clj-kondo.git")
             (commit "d8a6ee9a16aa24b5be01f1edf9843d41bdc75555")))
       (sha256
        (base32 "010gzxwvr2p2wv358r76ajkn48ilgmkmv7z6bckqbap0cjhrqq43"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-flycheck))
    (home-page "https://github.com/borkdude/flycheck-clj-kondo")
    (synopsis "Add clj-kondo linter to flycheck")
    (description
     "This package integrates clj-kondo with Emacs via flycheck.  To use it, add to
your init.el:

(require 'flycheck-clj-kondo)

Make sure the clj-kondo binary is on your path.  For installation instructions,
see https://github.com/borkdude/clj-kondo.")
    (license #f)))

(define-public emacs-flycheck-elsa
  (package
    (name "emacs-flycheck-elsa")
    (version "20200203.1758")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-elsa/flycheck-elsa.git")
             (commit "6274e6d5391bcdca46164b3238b045ca21c353a2")))
       (sha256
        (base32 "0dr52dqxsgswswbkdbv0dax57k41j58n5wf3gny6yz52626kv8n8"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-flycheck))
    (home-page "https://github.com/emacs-elsa/flycheck-elsa")
    (synopsis "Flycheck for Elsa.")
    (description "Flycheck integration for Elsa.  See README.md")
    (license #f)))

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
     " flymd is a on the fly markdown preview package.  It is super, super, super easy
to use.  Open a markdown file, M-x flymd-flyit The markdown file is opened in
your favourite browser.  When you finished, close the browser page and kill the
markdwon buffer.

Please go https://github.com/mola-T/flymd for more info")
    (license #f)))

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
     "Call `github-browse-file' (for the git blob) or `github-browse-file-blame' (for
the git blame) to view current file on GitHub.  With a prefix argument (C-u),
you can force them to use the \"master\" branch.")
    (license #f)))

(define-public emacs-guess-language
  (package
    (name "emacs-guess-language")
    (version "20210623.1505")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tmalsburg/guess-language.el.git")
             (commit "e6b78ed2a36bf5debd3d07ffd99a5a8ca60609d6")))
       (sha256
        (base32 "0g0vdz42s6hns249lfxcha7l7ihqpyay3n5iijziwrbrrhqi6rx6"))))
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
    (license #f)))

(define-public emacs-grip-mode
  (package
    (name "emacs-grip-mode")
    (version "20220228.1516")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/seagle0128/grip-mode.git")
             (commit "c0b45c3a0bf2f6ea51b166020aaf27050f4d3c66")))
       (sha256
        (base32 "1zcrzy47lxk5p6qk0d9x2gpy2pza8kjwcp7aqnad7gl6jr257cac"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/seagle0128/grip-mode")
    (synopsis "Instant GitHub-flavored Markdown/Org preview using grip.")
    (description
     "Instant GitHub-flavored Markdown/Org preview using a grip subprocess.

Install: From melpa, `M-x package-install RET grip-mode RET`.  ;; Make a
keybinding: `C-c C-c g' (define-key markdown-mode-command-map (kbd \"g\")
#'grip-mode) ;; or start grip when opening a markdown file (add-hook
'markdown-mode-hook #'grip-mode) or (use-package grip-mode   :ensure t   :bind
(:map markdown-mode-command-map          (\"g\" .  grip-mode))) Run `M-x
grip-mode` to preview the markdown file with the default browser.")
    (license #f)))

(define-public emacs-jiralib2
  (package
    (name "emacs-jiralib2")
    (version "20200520.2031")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nyyManni/jiralib2.git")
             (commit "c21c4e759eff549dbda11099f2f680b78d7f5a01")))
       (sha256
        (base32 "0yrcc9yfz9gxkhizy03bpysl1wcdbk0m6fj9hkqw3kbgnsk25h4p"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-request emacs-dash))
    (home-page "https://github.com/nyyManni/jiralib2")
    (synopsis "JIRA REST API bindings to Elisp")
    (description
     "This file provides a programatic interface to JIRA.  It provides access to JIRA
from other programs, but no user level functionality.

jiralib2 supports three methods of authentication: cookie, basic and token.
Cookie auth is the same which is used in the browser, and works by requesting a
session id from the API.  Basic auth works by including the Authorization-header
in all the requests.  Token authentication is similar to the basic
authentication, but uses a server-side generated token instead of the password,
and is only available with JIRA Cloud.  OAuth login is not supported.

Jira References:

Primary reference (on current Jira, only REST is supported):
https://docs.atlassian.com/jira/REST/cloud/")
    (license #f)))

(define-public emacs-js2-refactor
  (package
    (name "emacs-js2-refactor")
    (version "20210306.2003")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/js-emacs/js2-refactor.el.git")
             (commit "a0977c4ce1918cc266db9d6cd7a2ab63f3a76b9a")))
       (sha256
        (base32 "0pjadcb5i8g8wkaf4hvh270r5z3qcsksnpcq5gzacqbgz5j2qcaf"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-js2-mode
           emacs-s
           emacs-multiple-cursors
           emacs-dash
           emacs-s
           emacs-yasnippet))
    (home-page "unspecified")
    (synopsis "A JavaScript refactoring library for emacs.")
    (description
     "This is a collection of small refactoring functions to further the idea of a
JavaScript IDE in Emacs that started with js2-mode.

## Installation

Start by installing the dependencies:

 * js2-mode https://github.com/mooz/js2-mode/  * dash
https://github.com/magnars/dash.el  * multiple-cursors
https://github.com/magnars/multiple-cursors.el

It is also recommended to get
[expand-region](https://github.com/magnars/expand-region.el) to more easily mark
vars, method calls and functions for refactorings.

Then add this to your Emacs settings:

    (require 'js2-refactor)     (add-hook 'js2-mode-hook #'js2-refactor-mode)
 (js2r-add-keybindings-with-prefix \"C-c C-m\")

Note: I am working on a smoother installation path through package.el, but I
haven't had the time to whip this project into that sort of structure - yet.

## Usage

All refactorings start with `C-c C-m` and then a two-letter mnemonic shortcut.

 * `ee` is `expand-node-at-point`: Expand bracketed list according to node type
at point (array, object, function, call args).   * `cc` is
`contract-node-at-point`: Contract bracketed list according to node type at
point (array, object, function, call args).   * `ef` is `extract-function`:
Extracts the marked expressions out into a new named function.   * `em` is
`extract-method`: Extracts the marked expressions out into a new named method in
an object literal.   * `tf` is `toggle-function-expression-and-declaration`:
Toggle between function name() {} and var name = function ();  * `ta` is
`toggle-arrow-function-and-expression`: Toggle between function expression to
arrow function.   * `ts` is `toggle-function-async`: Toggle between an async and
a regular function.   * `ip` is `introduce-parameter`: Changes the marked
expression to a parameter in a local function.   * `lp` is `localize-parameter`:
Changes a parameter to a local var in a local function.   * `wi` is
`wrap-buffer-in-iife`: Wraps the entire buffer in an immediately invoked
function expression  * `ig` is `inject-global-in-iife`: Creates a shortcut for a
marked global by injecting it in the wrapping immediately invoked function
expression  * `ag` is `add-to-globals-annotation`: Creates a `/*global */`
annotation if it is missing, and adds the var at point to it.   * `ev` is
`extract-var`: Takes a marked expression and replaces it with a var.   * `el` is
`extract-var`: Takes a marked expression and replaces it with a let.   * `ec` is
`extract-var`: Takes a marked expression and replaces it with a const.   * `iv`
is `inline-var`: Replaces all instances of a variable with its initial value.
* `rv` is `rename-var`: Renames the variable on point and all occurrences in its
lexical scope.   * `vt` is `var-to-this`: Changes local `var a` to be `this.a`
instead.   * `ao` is `arguments-to-object`: Replaces arguments to a function
call with an object literal of named arguments.   * `3i` is `ternary-to-if`:
Converts ternary operator to if-statement.   * `sv` is `split-var-declaration`:
Splits a `var` with multiple vars declared, into several `var` statements.   *
`ss` is `split-string`: Splits a `string`.   * `st` is `string-to-template`:
Converts a `string` into a template string.   * `uw` is `unwrap`: Replaces the
parent statement with the selected region.   * `lt` is `log-this`: Adds a
console.log() statement for what is at point (or region).  With a prefix
argument, use JSON pretty-printing.   * `dt` is `debug-this`: Adds a debug()
statement for what is at point (or region).   * `sl` is `forward-slurp`: Moves
the next statement into current function, if-statement, for-loop or while-loop.
* `ba` is `forward-barf`: Moves the last child out of current function,
if-statement, for-loop or while-loop.   * `k` is `kill`: Kills to the end of the
line, but does not cross semantic boundaries.

There are also some minor conveniences bundled:

 * `C-S-down` and `C-S-up` moves the current line up or down.  If the line is an
  element in an object or array literal, it makes sure that the commas are
still correctly placed.   * `k` `kill-line`: Like `kill-line` but respecting the
AST.

## Todo

A list of some wanted improvements for the current refactorings.

 * expand- and contract-array: should work recursively with nested object
literals and nested arrays.   * expand- and contract-function: should deal
better with nested object literals, array declarations, and statements
terminated only by EOLs (without semicolons).   * wrap-buffer-in-iife: should
skip comments and namespace initializations at buffer start.   *
extract-variable: could end with a query-replace of the expression in its scope.

## Contributions

* [Matt Briggs](https://github.com/mbriggs) contributed
`js2r-add-to-globals-annotation` * [Alex
Chamberlain](https://github.com/apchamberlain) contributed contracting and
expanding arrays and functions.  * [Nicolas
Petton](https://github.com/NicolasPetton) contributed `js2r-kill` Thanks!

## Contribute

This project is still in its infancy, and everything isn't quite sorted out yet.
 If you're eager to contribute, please add an issue here on github and we can
discuss your changes a little before diving into the elisp :-).

To fetch the test dependencies:

    $ cd /path/to/multiple-cursors     $ git submodule init     $ git submodule
update

Run the tests with:

    $ ./util/ecukes/ecukes features")
    (license #f)))

(define-public emacs-json-process-client
  (package
    (name "emacs-json-process-client")
    (version "20230903.1305")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/DamienCassou/json-process-client.git")
                    (commit "c4385859ada9b7803698a1f0199fea7fc8880214")))
              (sha256 (base32
                       "1n4spfyv7g88mkvca0cxc34qvp3x8vc838hmyp7x4ijr87lp8inm"))))
    (build-system emacs-build-system)
    (home-page "https://gitlab.petton.fr/nico/json-process-client")
    (synopsis "Interact with a TCP process using JSON")
    (description
     "This library starts a process and communicates with it through JSON over TCP.
The process must output one JSON message per line.")
    (license #f)))

(define-public emacs-kubel
  (package
    (name "emacs-kubel")
    (version "20220104.2320")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abrochard/kubel.git")
             (commit "68d2925c7942039e3fb3eb6c113adec5369c6c72")))
       (sha256
        (base32 "173ympabfa14rc6y4f3rjxapj7py0dsnzp8zg7q2gkyxv1iwhh55"))))
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
    (license #f)))

(define-public emacs-language-detection
  (package
    (name "emacs-language-detection")
    (version "20161123.1813")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andreasjansson/language-detection.el.git")
             (commit "54a6ecf55304fba7d215ef38a4ec96daff2f35a4")))
       (sha256
        (base32 "0p8kim8idh7hg9398kpgjawkxq9hb6fraxpamdkflg8gjk0h5ppa"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/andreasjansson/language-detection.el")
    (synopsis "Automatic language detection from code snippets")
    (description
     "Automatic programming language detection using pre-trained random forest
classifier.

Supported languages:

 * ada  * awk  * c  * clojure  * cpp  * csharp  * css  * dart  * delphi  *
emacslisp  * erlang  * fortran  * fsharp  * go  * groovy  * haskell  * html  *
java  * javascript  * json  * latex  * lisp  * lua  * matlab  * objc  * perl  *
php  * prolog  * python  * r  * ruby  * rust  * scala  * shell  * smalltalk  *
sql  * swift  * visualbasic  * xml

Entrypoints:

 * language-detection-buffer    - When called interactively, prints the language
of the current      buffer to the echo area    - When called non-interactively,
returns the language of the      current buffer  * language-detection-string
- Non-interactive function, returns the language of its argument")
    (license #f)))

(define-public emacs-llm
  (package
    (name "emacs-llm")
    (version "0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ahyatt/llm.git")
                    (commit "23616e6cf597b2e5be6824b645751ca2b790ba3b")))
              (sha256 (base32
                       "0qm75rcbyh9kh91snlf35ivsb9s4lrvy71nhwj8sz2yf6xfim3fl"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ahyatt/llm")
    (synopsis "Interface to pluggable llm backends")
    (description
     "This is a library for interfacing with Large Language Models. It
allows elisp code to use LLMs, but allows gives the end-user an option
to choose which LLM they would prefer. This is especially useful for
LLMs, since there are various high-quality ones that in which API
access costs money, as well as locally installed ones that are free,
but of medium quality. Applications using LLMs can use this library to
make sure their application works regardless of whether the user has a
local LLM or is paying for API access.")
    (license license:gpl3+)))

(define-public emacs-lsp-dart
  (package
    (name "emacs-lsp-dart")
    (version "20220129.1427")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-lsp/lsp-dart.git")
             (commit "5cef1b6a34327bf236c8b7b23c44bdd968b4585b")))
       (sha256
        (base32 "0kvjkpv0yvahmlkqyzl9ayl3mmmr4gcmi3p5pp8092ah0xgys86p"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-lsp-treemacs
           emacs-lsp-mode
           emacs-dap-mode
           emacs-f
           emacs-dash
           emacs-dart-mode))
    (home-page "https://emacs-lsp.github.io/lsp-dart")
    (synopsis "Dart support lsp-mode")
    (description "Dart analysis server client for LSP mode")
    (license #f)))

(define-public emacs-lsp-docker
  (package
    (name "emacs-lsp-docker")
    (version "20211203.1659")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-lsp/lsp-docker.git")
             (commit "c2da2a65cb11e92d23c480dcc12387aa53997181")))
       (sha256
        (base32 "067bc37v14mvrmayah95qkcmi8gh3fdhdh8493wabm47kgszsfh4"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-dash emacs-lsp-mode emacs-f emacs-yaml emacs-ht))
    (home-page "https://github.com/emacs-lsp/lsp-docker")
    (synopsis "LSP Docker integration")
    (description "Run language servers in containers")
    (license #f)))

(define-public emacs-markdown-preview-eww
  (package
    (name "emacs-markdown-preview-eww")
    (version "20160111.1502")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/niku/markdown-preview-eww.git")
             (commit "5853f836425c877c8a956501f0adda137ef1d3b7")))
       (sha256
        (base32 "1i5gr3j9dq41p2zl4bfyvzv6i5z7hgrxzrycmbdc3s7nja36k9z4"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/niku/markdown-preview-eww")
    (synopsis "Realtime preview by eww")
    (description "This package provides the realtime markdown preview by eww.")
    (license #f)))

(define-public emacs-tblui
  (package
    (name "emacs-tblui")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Yuki-Inoue/tblui.el")
                    (commit "bb29323bb3e27093d50cb42db3a9329a096b6e4d")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pja9v4h3abqc2iydm7wwjxrg7ni1pn94yb4azrgjq5qc0fsgn7a"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash emacs-magit-popup emacs-tablist))
    (home-page "https://github.com/Yuki-Inoue/tblui.el")
    (synopsis "Define tabulated list mode based UIs more easily")
    (description "This package can be used to define user interfaces that are similar to
Emacs built-in tabulated list mode, but with less boilerplate.")
    (license license:gpl3+)))

(define-public emacs-openai
  (let ((commit "6a1d270fcdf89226f22122df930afe17f85d3209"))
    (package
      (name "emacs-openai")
      (version (git-version "0.1.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-openai/openai")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "06slixip262lxwl1vdk225yk9lg5ildnl123drm4rdl3yzapc4xn"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-dash emacs-request emacs-tblui))
      (home-page "https://github.com/emacs-openai/openai")
      (synopsis "Elisp library for the OpenAI API")
      (description "The OpenAI Elisp library provides convenient access to the OpenAI API
from applications written in the Elisp language.")
      (license license:gpl3+))))

(define-public emacs-chatgpt
  (let ((commit "73a1d192b2eae9fb65e0688d7c99e91bc4101d38"))
    (package
      (name "emacs-chatgpt")
      (version (git-version "0.1.0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-openai/chatgpt")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hvjqcngywjw109rma0mb7a79wbczr8hhfpz94krfwrs5vck9pxa"))))
      (build-system emacs-build-system)
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

(define-public emacs-codegpt
  (let ((commit "d5de204b6438eafeaa667d3007699f84ac87f5f9"))
    (package
      (name "emacs-codegpt")
      (version (git-version "0.1.0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-openai/codegpt")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0k10g64qcnz0w808plg81dydbc2bcfnvd4vzcwjv6djh0w93crbi"))))
      (build-system emacs-build-system)
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

(define-public emacs-dall-e
  (let ((commit "f8f93843a333fac63a0722cd4c710f0db408f28f"))
    (package
      (name "emacs-dall-e")
      (version (git-version "0.1.0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-openai/dall-e.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xvgzvkf5bw0mi3v7q19qq9mb6kgqm5kdr4b3dvasxyfjrqafbj0"))))
      (build-system emacs-build-system)
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
  (let ((commit "f62be2d11c8a9182cf84f0efe7ed054cc304262d"))
    (package
      (name "emacs-reveal-in-folder")
      (version (git-version "0.1.2" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jcs-elpa/reveal-in-folder.git")
                      (commit "648aef32c55c0b2b175ed0939fbe50fd7fa0dcf5")))
                (sha256
                 (base32
                  "0018rwsl6gqjxyavny2m9q66yxlbw86rjfi570zdhj0zmdh8yfxw"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-f emacs-s))
      (home-page "https://github.com/jcs-elpa/reveal-in-folder")
      (synopsis "Reveal current file in folder")
      (description "Reveal current file in folder.")
      (license #f))))

(define-public emacs-org-gcal
  (package
    (name "emacs-org-gcal")
    (version "20220119.2142")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kidd/org-gcal.el.git")
             (commit "6e26ae75aea521ea5dae67e34265da534bdad2d1")))
       (sha256
        (base32 "1814w5bgf9zwvsga4926i002q2xg2qgyrmb2jlkc7gzv0j86ccv9"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-request
           emacs-request-deferred
           emacs-alert
           emacs-persist
           emacs-org))
    (home-page "https://github.com/kidd/org-gcal.el")
    (synopsis "Org sync with Google Calendar")
    (description
     " Put the org-gcal.el to your load-path.  Add to .emacs: (require 'org-gcal)")
    (license #f)))

(define-public emacs-ox-jira
  (package
    (name "emacs-ox-jira")
    (version "20220121.1015")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stig/ox-jira.el.git")
             (commit "a8019237a8f5e016a7c952fc2f673a2498a0e779")))
       (sha256
        (base32 "0pa7pwk0yjcgak3f9w9jggj3ghlig1azf15ng954r646810j9i4v"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-org))
    (home-page "https://github.com/stig/ox-jira.el")
    (synopsis "JIRA Backend for Org Export Engine")
    (description
     "This module plugs into the regular Org Export Engine and transforms Org files to
JIRA markup for pasting into JIRA tickets & comments.

In an Org buffer, hit `C-c C-e j j' to bring up *Org Export Dispatcher* and
export it as a JIRA buffer.  I usually use `C-x h' to mark the whole buffer,
then `M-w' to save it to the kill ring (and global pasteboard) for pasting into
JIRA issues.")
    (license #f)))

(define-public emacs-request-deferred
  (package
    (name "emacs-request-deferred")
    (version "20210214.37")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tkf/emacs-request.git")
             (commit "3336eaa97de923f74b90dda3e35985e122d40805")))
       (sha256
        (base32 "0jckwy5zhz95d6l3lz8b9b34pppcjjzy97fg1wn8mqzhf3h460ac"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-deferred emacs-request))
    (arguments '(#:include '("^request-deferred.el$") #:exclude '()))
    (home-page "https://github.com/tkf/emacs-request")
    (synopsis "Wrap request.el by deferred")
    (description
     "Trivial wrapper to request library returing kiwanami deferred object.")
    (license #f)))

(define-public emacs-scss-mode
  (package
    (name "emacs-scss-mode")
    (version "20180123.1708")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/antonj/scss-mode.git")
             (commit "cf58dbec5394280503eb5502938f3b5445d1b53d")))
       (sha256
        (base32 "0raja19l0igwr0pn0ghr1pj1d8i9k3m3764ma4r8nwzxcj9qw4ja"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/antonj/scss-mode")
    (synopsis "Major mode for editing SCSS files")
    (description
     "Command line utility sass is required, see http://sass-lang.com/ To install
sass: gem install sass

Also make sure sass location is in emacs PATH, example: (setq exec-path (cons
(expand-file-name \"~/.gem/ruby/1.8/bin\") exec-path)) or customize\n`scss-sass-command' to point to your sass executable.")
    (license #f)))

(define-public emacs-smooth-scrolling
  (package
    (name "emacs-smooth-scrolling")
    (version "20161002.1949")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aspiers/smooth-scrolling.git")
             (commit "2462c13640aa4c75ab3ddad443fedc29acf68f84")))
       (sha256
        (base32 "1h15gjq781i6fsz32qlh51knawdr8hcqvshsz6cszp752cibdcdg"))))
    (build-system emacs-build-system)
    (home-page "http://github.com/aspiers/smooth-scrolling/")
    (synopsis "Make emacs scroll smoothly")
    (description
     "To interactively toggle the mode on / off:

    M-x smooth-scrolling-mode

To make the mode permanent, put this in your .emacs:

    (require 'smooth-scrolling)     (smooth-scrolling-mode 1)

This package offers a global minor mode which make emacs scroll smoothly.  It
keeps the point away from the top and bottom of the current buffer's window in
order to keep lines of context around the point visible as much as possible,
whilst minimising the frequency of sudden scroll jumps which are visually
confusing.

This is a nice alternative to all the native `scroll-*` custom variables, which
unfortunately cannot provide this functionality perfectly.  For example, when
using the built-in variables, clicking with the mouse in the margin will
immediately scroll the window to maintain the margin, so the text that you
clicked on will no longer be under the mouse.  This can be disorienting.  In
contrast, this mode will not do any scrolling until you actually move up or down
a line.

Also, the built-in margin code does not interact well with small windows.  If
the margin is more than half the window height, you get some weird behavior,
because the point is always hitting both the top and bottom margins.  This
package auto-adjusts the margin in each buffer to never exceed half the window
height, so the top and bottom margins never overlap.

See the README.md for more details.")
    (license #f)))

(define-public emacs-timesheet
  (package
    (name "emacs-timesheet")
    (version "20191024.151")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tmarble/timesheet.el.git")
             (commit "5098dc87d3d4f289b6c1b6532070dacbfe6de9fd")))
       (sha256
        (base32 "0wqxlb4a7fzf14629zw021216qyzz56xwr8hfh2fy6kj90m9br4c"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-s emacs-org emacs-auctex))
    (arguments '(#:include '("^[^/]+.el$" "^bin$" "^share$") #:exclude '()))
    (home-page "https://github.com/tmarble/timesheet.el")
    (synopsis "Timesheet management add-on for org-mode")
    (description
     "Debian-depends: emacs24 make gawk sed git tar rubber texlive-latex-extra
texlive-fonts-recommended texlive-fonts-extra evince

This library adds timetracking and invoice generation to org-mode and relies
heavily on org clocking http://orgmode.org/org.html#Clocking-work-time and TODO
items http://orgmode.org/org.html#TODO-Items and org spreadsheets
http://orgmode.org/org.html#The-spreadsheet

This library attempts to conform to packaging conventions:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html Bugs,
enhancements welcome!

; Usage

Ensure TEXINPUTS is set to (in your ~/.bashrc) export
TEXINPUTS=.:$HOME/.emacs.d/elpa/auctex-11.87.4/latex:

Start by creating an example client...    M-x timesheet-example   You will be
viewing the buffer yoyodyne.org that already has some example   time entries...
Create an invoice with   M-x timesheet-invoice-this

Next steps...  - customize your name (in defs.tex) and logo (in logo.pdf).  -
update some time entries.

Example key bindings  see example.emacs.d/foo/bindings.el")
    (license #f)))

(define-public emacs-paimon
  (let ((commit "4c71bea56d0cfdca8ea90c5a6d8d584647d053af"))
    (package
      (name "emacs-paimon")
      (version (git-version "0.1.6" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/r0man/paimon.el.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bb66rzr3hzz9xyxw80vh7vrlsb6630n401dim47qqbi5xzbgr45"))))
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
                 (for-each (lambda (f) (rename-file f (basename f))) el-files)))))))
      (home-page "https://github.com/r0man/paimon.el")
      (synopsis "A major mode for Splunk")
      (description "This package provides a major mode for Splunk")
      (license #f))))

(define-public emacs-popwin
  (package
    (name "emacs-popwin")
    (version "20210215.1849")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacsorphanage/popwin.git")
             (commit "1184368d3610bd0d0ca4a3db4068048c562c2b50")))
       (sha256
        (base32 "0inm6wbfkw6b9bwikd77d0zmk6ma9fzfs11acblp5imq202v76ra"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/emacsorphanage/popwin")
    (synopsis "Popup Window Manager")
    (description
     "Popwin makes you free from the hell of annoying buffers such like *Help*,
*Completions*, *compilation*, and etc.

To use popwin, just add the following code into your .emacs:

    (require 'popwin)     (popwin-mode 1)

Then try to show some buffer, for example *Help* or *Completeions*.  Unlike
standard behavior, their buffers may be shown in a popup window at the bottom of
the frame.  And you can close the popup window seamlessly by typing C-g or
selecting other windows.

`popwin:display-buffer' displays special buffers in a popup window and displays
normal buffers as unsual.  Special buffers are specified in
`popwin:special-display-config', which tells popwin how to display such buffers.
 See docstring of `popwin:special-display-config' for more information.

The default width/height/position of popup window can be changed by setting
`popwin:popup-window-width', `popwin:popup-window-height', and
`popwin:popup-window-position'.  You can also change the behavior for a specific
buffer.  See docstring of `popwin:special-display-config'.

If you want to use some useful commands such like `popwin:popup-buffer' and
`popwin:find-file' easily.  You may bind `popwin:keymap' to `C-z', for example,
like:

    (global-set-key (kbd \"C-z\") popwin:keymap)

See also `popwin:keymap' documentation.

Enjoy!")
    (license #f)))

(define-public emacs-sayid
  (package
    (name "emacs-sayid")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/clojure-emacs/sayid/archive"
                                  "/refs/tags/v" version ".tar.gz"))
              (sha256
               (base32
                "1bscg0s5rv62p8a8pmniyqadrbvrq3lj1kza5lbp00ds3w55y0w1"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'enter-lisp-directory
            (lambda _
              (chdir "src/el"))))))
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

(define-public emacs-sql-indent
  (package
    (name "emacs-sql-indent")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/sql-indent-"
                                  version ".tar"))
              (sha256
               (base32
                "000pimlg0k4mrv2wpqq8w8l51wpr1lzlaq6ai8iaximm2a92ap5b"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/alex-hhh/emacs-sql-indent")
    (synopsis "Support for indenting code in SQL files.")
    (description
     "`sqlind-minor-mode is a minor mode that enables syntax-based indentation for
`sql-mode buffers: the TAB key indents the current line based on the SQL code on
previous lines.  To setup syntax-based indentation for every SQL buffer, add
`sqlind-minor-mode to `sql-mode-hook'.  Indentation rules are flexible and can
be customized to match your personal coding style.  For more information, see
the \"sql-indent.org\" file.  The package also defines align rules so that the
`align function works for SQL statements, see `sqlind-align-rules'.")
    (license license:gpl3+)))

(define-public emacs-sqlite3
  (let ((version "0.0.1")
        (revision "0")
        (commit "50479fc43df0fbc17cd02108f52fcf6e1686e907"))
    (package
      (name "emacs-sqlite3")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/syohex/emacs-sqlite3")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0xxaj8ah87wc416ncbp1f9zfhqa5yy2fpvlhz25fs5nhy1i8pzc6"))))
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
  (let ((version "0.0.1")
        (revision "0")
        (commit "88dfeae8f9612cb3564a7393aa8a5c867aacbaf8"))
    (package
      (name "emacs-sqlite3-api")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pekingduck/emacs-sqlite3-api")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0sj1fsgcgfzz6dfpmp8r5gmdwpbdzpk5g5lm8j7c3nqj6wqgg7g6"))))
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
                  `(module-load ,(string-append (assoc-ref outputs "out") "/lib/sqlite3-api.so"))))))
           (add-before 'install 'build-emacs-module
             ;; Run make.
             (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
               ;; Compile the shared object file.
               (apply invoke "make" "CC=gcc" make-flags)
               ;; Move the file into /lib.
               (install-file "sqlite3-api.so" (string-append (assoc-ref outputs "out") "/lib")))))
         #:tests? #f))
      (native-inputs
       (list libtool sqlite))
      (home-page "https://github.com/pekingduck/emacs-sqlite3-api")
      (synopsis "SQLite3 API for GNU Emacs 25+")
      (description "SQLite3 is a dynamic module for GNU Emacs 25+ that provides direct access to the core SQLite3 C API from Emacs Lisp.")
      (license license:gpl3+))))

(define-public emacs-virtualenvwrapper
  (package
    (name "emacs-virtualenvwrapper")
    (version "20190223.1919")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/porterjamesj/virtualenvwrapper.el.git")
             (commit "c7e84505db4142fd1beebf38ffe37c3f42444ca3")))
       (sha256
        (base32 "0kkhbw8c9c7nfhz7i0wwwcrbk0a91yvq7n5n89ndsk5iwisr92vp"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash emacs-s))
    (home-page "http://github.com/porterjamesj/virtualenvwrapper.el")
    (synopsis "a featureful virtualenv tool for Emacs")
    (description
     "This package provides a featureful virtualenv tool for Emacs.  Emulates much of
the functionality of Doug Hellmann's
[virtualenvwrapper](https://bitbucket.org/dhellmann/virtualenvwrapper/) See
documentation at https://github.com/porterjamesj/virtualenvwrapper.el for more
details.")
    (license #f)))

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
     " This is a major-mode for modelling and editing sequence-diagrams using the
syntax employed by the online service www.websequencediagrams.com.

The mode supports inline rendering the diagrams through the API provided through
the website and persisting these to image-files next to the files used to
generate them.

It will automatically activate for files with a WSD-extension.


Features:

- syntax higlighting of reccognized keywords - automatic indentation of
block-statements - generating and saving diagrams generated through WSD's online
API.  - support for WSD premium features (svg-export, etc) if API-key is
provided.  - rendering diagrams inline in Emacs, or in external OS viewer if
image   format is not supported by Emacs.


Customization:

To create mode-specific emacs-customizations, please use the wsd-mode-hook.

A short summary of customizable variables:

- wsd-api-key (default blank.  required for premium-features.) - wsd-format
(default png.  svg requires premium, thus api-key.) - wsd-style (default
modern-blue) - wsd-indent-offset (default 4) - wsd-font-lock-keywords")
    (license #f)))

(define-public emacs-x509-mode
  (package
    (name "emacs-x509-mode")
    (version "20210407.627")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jobbflykt/x509-mode.git")
             (commit "470769edba111aed8eabce58a3f2a02da0767624")))
       (sha256
        (base32 "19xvfzmsnc271a2zhjbnspb269c5mnps4l94ifrdlqn7y886qr4r"))))
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
         "^[^/]+.txt$")
       #:exclude
       '("^.dir-locals.el$"
         "^test.el$"
         "^tests.el$"
         "^[^/]+-test.el$"
         "^[^/]+-tests.el$")))
    (home-page "https://github.com/jobbflykt/x509-mode")
    (synopsis "View certificates, CRLs and keys using OpenSSL.")
    (description
     "Major for viewing certificates, CRLs, keys and DH-parameters.

Uses OpenSSL for viewing PEM and DER encoded PKI entities.

Usage: Open a file containing a certificate, either PEM or DER encode.  Now use
M-x `x509-viewcert' to create a new buffer that displays the decoded
certificate.  Use M-x `x509-viewcrl', M-X `x509-viewasn1', M-x `x509-viewkey'
and M-x `x509-viewdh' in a similar manner.")
    (license #f)))

(define-public emacs-ox-tufte
  (package
   (name "emacs-ox-tufte")
   (version "20231022.2117")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/ox-tufte/ox-tufte.git")
                   (commit "58422fb109f2b2a997f9c773b5436e7b62182e12")))
             (sha256 (base32
                      "14i1pliifj5p0i1bgsdgph32ilj7snrh8gnhk59f1f4ngh3kw3zg"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-org))
   (arguments '(#:include '("^[^/]+.el$" "^[^/]+.el.in$"
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
   (synopsis "Tufte HTML org-mode export backend")
   (description
    "This is an export backend for Org-mode that exports buffers to HTML that is
compatible with Tufte CSS - <https://edwardtufte.github.io/tufte-css/>.  The
design goal is to \"minimally\" change the HTML structure as generated by `ox-html
(with additional CSS as needed) to get behaviour that is equivalent to Tufte
CSS.")
   (license #f)))

(define-public emacs-consult-gh
  (let ((commit "a035eac54a3be270168e86f32075e5f6f426c103"))
    (package
      (name "emacs-consult-gh")
      (version (git-version "0.12" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/armindarvish/consult-gh")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1lnxnf0dc5wp3lkildib8p5iyzramsxdldqzgm1w149wqimyp7m9"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-consult emacs-embark))
      (home-page "https://github.com/armindarvish/consult-gh")
      (synopsis "An Interactive interface for GitHub CLI client inside GNU Emacs using Consult")
      (description "Consult-GH provides an interface to interact with GitHub
repositories (search, view files and issues, clone, fork, etc) from
inside Emacs. It uses the consult package and the GitHub CLI and
optionally Embark and provides an intuitive UI using minibuffer
completion familiar to Emacs users.")
      (license license:gpl3+))))

(define-public emacs-eglot-java
  (package
    (name "emacs-eglot-java")
    (version "20231228.2257")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/yveszoundi/eglot-java.git")
                    (commit "1abf8d547aaedce7307f3bd86145af0f2054bc9a")))
              (sha256 (base32
                       "0mnd0wy1kdnfniw1dkw1i7c623kmh4p0jj2pdf3q6xn51677209x"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eglot emacs-jsonrpc))
    (home-page "https://github.com/yveszoundi/eglot-java")
    (synopsis "Eglot Java package for Emacs")
    (description "Java extension for the Eglot LSP client")
    (license #f)))

(define-public emacs-whisper
  (package
    (name "emacs-whisper")
    (version "20231114.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/natrys/whisper.el")
             (commit "fb0ab898b2d01f7d5c5b2dd4379121c1dcfdbae1")))
       (sha256
        (base32 "0ci72s9qsgp224ms2mg0jbp7m6wl8s0d5wrl1pj3gawmikbpmf1w"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/natrys/whisper.el")
    (synopsis "Speech-to-Text interface for Emacs using OpenAI's whisper model and
whisper.cpp as inference engine.")
    (description
     "Speech-to-Text interface for Emacs using OpenAI’s whisper speech
recognition model. For the inference engine it uses the awesome C/C++
port whisper.cpp that can run on consumer grade CPU (without requiring
a high end GPU).")
    (license license:gpl3+)))
