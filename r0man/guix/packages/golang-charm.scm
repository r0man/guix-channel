(define-module (r0man guix packages golang-charm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module ((gnu packages golang-xyz)
                 #:hide (go-github-com-charmbracelet-colorprofile
                         go-github-com-charmbracelet-x-ansi
                         go-github-com-charmbracelet-x-cellbuf
                         go-github-com-charmbracelet-x-term
                         go-github-com-charmbracelet-x-windows))
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public go-github-com-clipperhouse-uax29-v2
  (package
    (name "go-github-com-clipperhouse-uax29-v2")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clipperhouse/uax29")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p18s46jd4ryqp036cyv4j6ys67706kihw0fj5ym98xf1m2mdsgg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "github.com/clipperhouse/uax29/v2"))
    (home-page "https://github.com/clipperhouse/uax29")
    (synopsis "Unicode text segmentation for Go")
    (description
     "This package provides Unicode text segmentation (UAX #29) for words,
sentences, and graphemes in Go.")
    (license license:expat)))

(define-public go-github-com-clipperhouse-displaywidth
  (package
    (name "go-github-com-clipperhouse-displaywidth")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clipperhouse/displaywidth")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "032f33vf5ign78l9clc3vz1kzirxgalxswm3j6l4nbf46vpp08yz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "github.com/clipperhouse/displaywidth"))
    (propagated-inputs (list go-github-com-clipperhouse-uax29-v2))
    (home-page "https://github.com/clipperhouse/displaywidth")
    (synopsis "Monospace display width for strings")
    (description
     "This package measures the monospace display width of strings, UTF-8
bytes, and runes in Go.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-ansi
  (package
    (name "go-github-com-charmbracelet-x-ansi")
    (version "0.11.6")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version
                                          #:subdir "ansi"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jlc7i41gv8gp4p452kv3f5gabpcix1mbbjfslpzj1k8lynb3nfc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "github.com/charmbracelet/x/ansi"
      #:unpack-path "github.com/charmbracelet/x"))
    (propagated-inputs (list go-github-com-bits-and-blooms-bitset
                             go-github-com-clipperhouse-displaywidth
                             go-github-com-clipperhouse-uax29-v2
                             go-github-com-lucasb-eyer-go-colorful
                             go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "ANSI escape sequences for terminal applications")
    (description
     "This package defines common ANSI escape sequences based on the ECMA-48
specs for terminal applications.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-term
  (package
    (name "go-github-com-charmbracelet-x-term")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version
                                          #:subdir "term"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gah6pnr4l7ap99haiqmn33csh4zqsls176nih2pn6hxm6089fij"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "github.com/charmbracelet/x/term"
      #:unpack-path "github.com/charmbracelet/x"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Terminal utilities for Go")
    (description
     "This package provides terminal utility functions for Go applications.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-windows
  (package
    (name "go-github-com-charmbracelet-x-windows")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version
                                          #:subdir "windows"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cvpyks5pnfsb7ifm2bdd1jaw50q39bdf7vj4lnhjyw1l62xn5v0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "github.com/charmbracelet/x/windows"
      #:unpack-path "github.com/charmbracelet/x"))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Windows terminal utilities for Go")
    (description
     "This package provides Windows-specific terminal utilities for Go.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-colorprofile
  (package
    (name "go-github-com-charmbracelet-colorprofile")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/colorprofile")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r9zsaq94is33rdbfr42lsdgk36fhlpdmc9m336nmj5f4g3pspb6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "github.com/charmbracelet/colorprofile"))
    ;; Note: charmbracelet/x/ansi and x/term omitted to avoid monorepo
    ;; source conflicts when used alongside other x/* packages.
    (propagated-inputs (list go-github-com-xo-terminfo))
    (home-page "https://github.com/charmbracelet/colorprofile")
    (synopsis "Terminal color profile detection for Go")
    (description
     "This package provides terminal color profile detection for Go
applications.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-cellbuf
  (package
    (name "go-github-com-charmbracelet-x-cellbuf")
    (version "0.0.15")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version
                                          #:subdir "cellbuf"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17f37m2zzhxcm422h143khvhfmh2k1jc6mqhrz6xqmhibqvgwh3a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "github.com/charmbracelet/x/cellbuf"
      #:unpack-path "github.com/charmbracelet/x"))
    ;; Note: propagated-inputs omit charmbracelet/x subpackages and
    ;; colorprofile (which propagates x/ansi) to avoid monorepo source
    ;; conflicts.  Consumers must add those explicitly.
    (propagated-inputs (list go-github-com-lucasb-eyer-go-colorful
                             go-github-com-rivo-uniseg))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Cell buffer for terminal rendering")
    (description
     "This package provides a cell buffer implementation for terminal
rendering in Go applications.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-ultraviolet
  (package
    (name "go-github-com-charmbracelet-ultraviolet")
    (version "0.0.0-20260205113103-524a6607adb8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/ultraviolet")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xxkish6mparlgbswb1070qnyl138bfj6339c3x91cdfm3q91c3l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "github.com/charmbracelet/ultraviolet"))
    (propagated-inputs (list go-github-com-charmbracelet-colorprofile
                             go-github-com-charmbracelet-x-ansi
                             go-github-com-charmbracelet-x-term
                             go-github-com-charmbracelet-x-termios
                             go-github-com-charmbracelet-x-windows
                             go-github-com-clipperhouse-uax29-v2
                             go-github-com-lucasb-eyer-go-colorful
                             go-github-com-muesli-cancelreader
                             go-github-com-rivo-uniseg
                             go-github-com-xo-terminfo))
    (home-page "https://github.com/charmbracelet/ultraviolet")
    (synopsis "Terminal rendering engine for Charm")
    (description
     "This package provides a terminal rendering engine used by Charm TUI
libraries.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-exp-ordered
  (package
    (name "go-github-com-charmbracelet-x-exp-ordered")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version
                                          #:subdir "exp/ordered"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01rr26pjhz2ms7bbqr21bs4vs1f60ix5y6gsp25sisvm36ds484l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "github.com/charmbracelet/x/exp/ordered"
      #:unpack-path "github.com/charmbracelet/x"))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Ordered constraints for Go generics")
    (description
     "This package provides ordered constraints for Go generics from the
Charm experimental packages.")
    (license license:expat)))

(define-public go-charm-land-lipgloss-v2
  (package
    (name "go-charm-land-lipgloss-v2")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/lipgloss")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02zfnqgj6x92yyri9a9ividd72h2v4nj4ig0c6h50w92x6lp2bms"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "charm.land/lipgloss/v2"))
    (propagated-inputs (list go-github-com-aymanbagabas-go-udiff
                             go-github-com-charmbracelet-colorprofile
                             go-github-com-charmbracelet-ultraviolet
                             go-github-com-charmbracelet-x-ansi
                             go-github-com-charmbracelet-x-exp-golden
                             go-github-com-charmbracelet-x-term
                             go-github-com-clipperhouse-displaywidth
                             go-github-com-lucasb-eyer-go-colorful
                             go-github-com-rivo-uniseg
                             go-golang-org-x-sys))
    (home-page "https://charm.land/lipgloss")
    (synopsis "Style definitions for terminal layouts")
    (description
     "This package provides style definitions for nice terminal layouts,
built with TUIs in mind.")
    (license license:expat)))

(define-public go-charm-land-bubbletea-v2
  (package
    (name "go-charm-land-bubbletea-v2")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/bubbletea")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mh9bqvkmc1dwqglk3j1qv4mwprqdn9sd3cr54jalc8d8mvs46iv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "charm.land/bubbletea/v2"))
    (propagated-inputs (list go-github-com-charmbracelet-colorprofile
                             go-github-com-charmbracelet-ultraviolet
                             go-github-com-charmbracelet-x-ansi
                             go-github-com-charmbracelet-x-exp-golden
                             go-github-com-charmbracelet-x-term
                             go-github-com-lucasb-eyer-go-colorful
                             go-github-com-muesli-cancelreader
                             go-golang-org-x-sys))
    (home-page "https://charm.land/bubbletea")
    (synopsis "TUI framework based on The Elm Architecture")
    (description
     "This package provides a framework for building rich terminal user
interfaces based on the paradigms of The Elm Architecture.")
    (license license:expat)))

(define-public go-charm-land-bubbles-v2
  (package
    (name "go-charm-land-bubbles-v2")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/bubbles")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03ksng6l2py25vcgfnkkqmccbl5scdgw4bggmkci9y16grz6441j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "charm.land/bubbles/v2"))
    (propagated-inputs (list go-charm-land-bubbletea-v2
                             go-charm-land-lipgloss-v2
                             go-github-com-atotto-clipboard
                             go-github-com-charmbracelet-harmonica
                             go-github-com-charmbracelet-x-ansi
                             go-github-com-charmbracelet-x-exp-golden
                             go-github-com-dustin-go-humanize
                             go-github-com-makenowjust-heredoc
                             go-github-com-mattn-go-runewidth
                             go-github-com-rivo-uniseg
                             go-github-com-sahilm-fuzzy))
    (home-page "https://charm.land/bubbles")
    (synopsis "TUI components for Bubble Tea applications")
    (description
     "This package provides TUI components for Bubble Tea applications
including text inputs, spinners, paginated lists, and more.")
    (license license:expat)))

(define-public go-charm-land-glamour-v2
  (package
    (name "go-charm-land-glamour-v2")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/glamour")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02sw0v7h2j3ch74mbz68wafzlxfq4jx2qvjvw5vhgqsy72yymbry"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "charm.land/glamour/v2"))
    (propagated-inputs (list go-charm-land-lipgloss-v2
                             go-github-com-alecthomas-chroma-v2
                             go-github-com-charmbracelet-x-ansi
                             go-github-com-charmbracelet-x-exp-golden
                             go-github-com-charmbracelet-x-exp-slice
                             go-github-com-microcosm-cc-bluemonday
                             go-github-com-yuin-goldmark
                             go-github-com-yuin-goldmark-emoji
                             go-golang-org-x-text))
    (home-page "https://charm.land/glamour")
    (synopsis "Render markdown on ANSI terminals")
    (description
     "This package renders markdown documents and templates on ANSI
compatible terminals with customizable stylesheets.")
    (license license:expat)))

(define-public go-charm-land-huh-v2
  (package
    (name "go-charm-land-huh-v2")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/huh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zqvq6xpn6jgzr58qwj8ywf2rymc0m0kgn9gg3p1gpqskk1fw992"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:skip-build? #t
      #:import-path "charm.land/huh/v2"))
    (propagated-inputs (list go-charm-land-bubbles-v2
                             go-charm-land-bubbletea-v2
                             go-charm-land-lipgloss-v2
                             go-github-com-catppuccin-go
                             go-github-com-charmbracelet-x-ansi
                             go-github-com-charmbracelet-x-exp-ordered
                             go-github-com-charmbracelet-x-exp-strings
                             go-github-com-charmbracelet-x-term
                             go-github-com-charmbracelet-x-xpty
                             go-github-com-mitchellh-hashstructure-v2))
    (home-page "https://charm.land/huh")
    (synopsis "Terminal forms and prompts library for Go")
    (description
     "This package provides components to build terminal-based forms and
prompts with an interactive and user-friendly interface.")
    (license license:expat)))
