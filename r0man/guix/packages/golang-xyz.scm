(define-module (r0man guix packages golang-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (r0man guix packages golang-maths))

(define-public go-github-com-aleksi-pointer
  (package
    (name "go-github-com-aleksi-pointer")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AlekSi/pointer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rncmanv62wwy7ihvjhvb695mkvy4kdzhgnn90zygs9yrfzzk0wk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/AlekSi/pointer"))
    (home-page "https://github.com/AlekSi/pointer")
    (synopsis "Go package for pointer helpers")
    (description
     "Package pointer provides helper routines for simplifying the creation of
pointer types for literals of basic Go types.")
    (license license:expat)))

(define-public go-github-com-olebedev-when
  (package
    (name "go-github-com-olebedev-when")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olebedev/when")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11jpz8w12pip6qy5ylyd7jx5r2imd35q1xkay296gj4w2k2g1yh6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/olebedev/when"
      ;; Tests require testify which has many transitive dependencies
      #:tests? #f))
    (propagated-inputs (list go-github-com-aleksi-pointer
                             go-github-com-pkg-errors))
    (home-page "https://github.com/olebedev/when")
    (synopsis "Natural language date/time parser for Go")
    (description
     "Package when provides a natural language date/time parser with pluggable
rules and merge strategies.  It supports parsing expressions like \"tomorrow\",
\"next week\", and \"in 2 hours\".")
    (license license:expat)))

(define-public go-github-com-inconshreveable-mousetrap
  (package
    (name "go-github-com-inconshreveable-mousetrap")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/inconshreveable/mousetrap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14gjpvwgx3hmbd92jlwafgibiak2jqp25rq4q50cq89w8wgmhsax"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/inconshreveable/mousetrap"))
    (home-page "https://github.com/inconshreveable/mousetrap")
    (synopsis "Detect Windows executable started from terminal")
    (description
     "Mousetrap detects whether a Go program is started from Windows
explorer or cmd/powershell.")
    (license license:asl2.0)))

(define-public go-github-com-tetratelabs-wazero
  (package
    (name "go-github-com-tetratelabs-wazero")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wazero/wazero")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15gpb9w2wq16z40042vkhqzs39yb6icpxd4l8cnwhdfzbj3rd1hm"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (for-each delete-file-recursively
                      (list
                       ;; This directory holds the wazero site's source code.
                       "site"
                       ;; Windows related MSI packaging files.
                       "packaging"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tetratelabs/wazero"
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestHugePageConfigs"
                             "TestRun"
                             "TestRun/3_1"
                             "Test_cli"
                             "Test_cli/cargo-wasi"
                             "Test_cli/cargo-wasi/test.txt"
                             "Test_cli/cargo-wasi/testcases/test.txt"
                             "Test_cli/tinygo"
                             "Test_cli/tinygo/test.txt"
                             "Test_cli/tinygo/testcases/test.txt"
                             "Test_cli/zig"
                             "Test_cli/zig-cc"
                             "Test_cli/zig-cc/test.txt"
                             "Test_cli/zig-cc/testcases/test.txt"
                             "Test_cli/zig/test.txt")
                       "|"))))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/wazero/wazero")
    (synopsis "CGO-free WebAssembly runtime for Go")
    (description
     "wazero is a WebAssembly Core Specification
@url{https://www.w3.org/TR/2019/REC-wasm-core-1-20191205/,1.0} and
@url{https://www.w3.org/TR/2022/WD-wasm-core-2-20220419/,2.0} compliant
runtime.  It doesn't rely on CGO, which means you can run applications in
other languages and still keep cross compilation.")
    (license license:asl2.0)))

(define-public go-github-com-ncruces-go-sqlite3
  (package
    (name "go-github-com-ncruces-go-sqlite3")
    (version "0.30.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncruces/go-sqlite3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s7xlc5dynj5mcb7p9s1iazii668ph3v0wiz7fkmci83pgiknknl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ncruces/go-sqlite3"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                ;; Skip tests that fail due to build environment constraints
                (invoke "go" "test"
                        "github.com/ncruces/go-sqlite3"
                        "github.com/ncruces/go-sqlite3/embed/..."
                        "github.com/ncruces/go-sqlite3/ext/array"
                        "github.com/ncruces/go-sqlite3/ext/blobio"
                        "github.com/ncruces/go-sqlite3/ext/csv"
                        "github.com/ncruces/go-sqlite3/ext/ipaddr"
                        "github.com/ncruces/go-sqlite3/ext/lines"
                        "github.com/ncruces/go-sqlite3/ext/pivot"
                        "github.com/ncruces/go-sqlite3/ext/regexp"
                        "github.com/ncruces/go-sqlite3/ext/statement"
                        "github.com/ncruces/go-sqlite3/ext/stats"
                        "github.com/ncruces/go-sqlite3/internal/..."
                        "github.com/ncruces/go-sqlite3/util/..."
                        "github.com/ncruces/go-sqlite3/vfs/memdb"
                        "github.com/ncruces/go-sqlite3/vfs/mvcc"
                        "github.com/ncruces/go-sqlite3/vfs/xts")))))))
    (native-inputs
     (list go-github-com-dchest-siphash
           go-github-com-google-uuid
           go-golang-org-x-crypto
           go-golang-org-x-text))
    (propagated-inputs
     (list go-github-com-ncruces-julianday
           go-github-com-ncruces-sort
           go-github-com-ncruces-wbt
           go-github-com-tetratelabs-wazero
           go-golang-org-x-sys))
    (home-page "https://github.com/ncruces/go-sqlite3")
    (synopsis "Go SQLite database driver")
    (description
     "This package provides a low-level Go interface to SQLite.  It is a
SQLite driver that does not use cgo, making it suitable for WASM and other
environments where cgo is not available.")
    (license license:expat)))

(define-public go-github-com-ncruces-go-strftime
  (package
    (name "go-github-com-ncruces-go-strftime")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncruces/go-strftime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rmr44m8mj5w9j1sy4c24b3n55lx2gwz1z3lb2g3p4qw87wv0j2g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ncruces/go-strftime"))
    (home-page "https://github.com/ncruces/go-strftime")
    (synopsis "C99-compatible strftime for Go")
    (description
     "This package provides a C99-compatible strftime implementation for Go.")
    (license license:expat)))

(define-public go-github-com-ncruces-julianday
  (package
    (name "go-github-com-ncruces-julianday")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncruces/julianday")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12zg7hwbhvw2ns2qnf86d4nk0vkbklvp3h49ins7qb43iqw73s8i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ncruces/julianday"))
    (home-page "https://github.com/ncruces/julianday")
    (synopsis "Julian Day calculator for Go")
    (description
     "This package provides a Julian Day calculator for Go, compatible with
SQLite's date and time functions.")
    (license license:expat)))

(define-public go-github-com-ncruces-sort
  (package
    (name "go-github-com-ncruces-sort")
    (version "0.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncruces/sort")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07kamgkqhhn1klybx9z11a5pznra89pk1316b5x53jvj3116qjyn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ncruces/sort"
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (delete 'build))))
    (home-page "https://github.com/ncruces/sort")
    (synopsis "Sorting algorithms for Go")
    (description
     "This package provides sorting algorithms implemented in Go, including
Quicksort, Heapsort, and Shellsort.  Designed for educational purposes with
clarity and simplicity as primary goals.")
    (license license:expat)))

(define-public go-github-com-ncruces-wbt
  (package
    (name "go-github-com-ncruces-wbt")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncruces/wbt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0simy6xbyjhhq9pdjjw4whrsgv7b1rv1fd7v0dljs5qp14bms8jj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ncruces/wbt"))
    (home-page "https://github.com/ncruces/wbt")
    (synopsis "Immutable weight-balanced trees for Go")
    (description
     "This package provides a persistent implementation of weight-balanced
trees for Go, including join-based tree algorithms and order statistics.")
    (license license:expat)))

(define-public go-modernc-org-memory
  (package
    (name "go-modernc-org-memory")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/memory")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wvhv15f7f229gb2imr8lg8yh14kxh6z7kiywv91bbzgrcbrnk1j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/memory"
      #:unpack-path "modernc.org/memory"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-modernc-org-mathutil))
    (home-page "https://modernc.org/memory")
    (synopsis "Memory allocator for Go")
    (description
     "Package memory provides low-level memory allocation and management
utilities.")
    (license license:bsd-3)))

(define-public go-modernc-org-libc
  (package
    (name "go-modernc-org-libc")
    (version "1.66.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/libc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01xj0flqcy5sx75ddcgjvwilbi87gn9wy33k76p88m5f820ldf4d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/libc"
      #:unpack-path "modernc.org/libc"
      #:tests? #f))  ; Tests require special setup
    (propagated-inputs
     (list go-github-com-dustin-go-humanize
           go-github-com-google-uuid
           go-github-com-ncruces-go-strftime
           go-github-com-remyoudompheng-bigfft
           go-golang-org-x-exp
           go-golang-org-x-sys
           go-modernc-org-mathutil
           go-modernc-org-memory))
    (home-page "https://modernc.org/libc")
    (synopsis "C standard library implementation in Go")
    (description
     "Package libc provides a C standard library implementation for use with
ccgo-generated code.")
    (license license:bsd-3)))

(define-public go-modernc-org-sqlite
  (package
    (name "go-modernc-org-sqlite")
    (version "1.38.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/sqlite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0579vip4vn488jppjpadryxyimkw2jr8ywr4j0piqcm2zs40h509"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/sqlite"
      #:unpack-path "modernc.org/sqlite"
      #:tests? #f))  ; Tests require special setup
    (propagated-inputs
     (list go-github-com-dustin-go-humanize
           go-github-com-google-uuid
           go-github-com-ncruces-go-strftime
           go-github-com-remyoudompheng-bigfft
           go-golang-org-x-sys
           go-modernc-org-libc
           go-modernc-org-mathutil
           go-modernc-org-memory))
    (home-page "https://modernc.org/sqlite")
    (synopsis "CGo-free port of SQLite")
    (description
     "Package sqlite is a CGo-free port of SQLite.  It is a drop-in
replacement for mattn/go-sqlite3.")
    (license license:bsd-3)))

(define-public go-github-com-catppuccin-go
  (package
    (name "go-github-com-catppuccin-go")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/catppuccin/go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gqbgsap69abq1jdc4dhxvsrfb9yqc9423x6za6d7slfky1f736d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/catppuccin/go"))
    (home-page "https://github.com/catppuccin/go")
    (synopsis "Catppuccin color palette for Go")
    (description
     "This package provides the Catppuccin color palette for Go applications.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-errors
  (package
    (name "go-github-com-charmbracelet-x-errors")
    (version "0.0.0-20251028133951-21a390f3cede")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version #:subdir "errors"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "088786ak8jsgvssbb5y16z066vfq9f6078sjc7iv766knfr2i0x2"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "errors")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/x/errors"
      #:unpack-path "github.com/charmbracelet/x"))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Error handling utilities for Go")
    (description
     "This package provides error handling utilities for Go applications.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-termios
  (package
    (name "go-github-com-charmbracelet-x-termios")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version #:subdir "termios"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17nc4wz65j6kmxg00ww44qiqhgzkkr5cl0p7svq8qv3phpxy2kj7"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "termios")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/x/termios"
      #:unpack-path "github.com/charmbracelet/x"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Terminal I/O settings for Go")
    (description
     "This package provides utilities for working with terminal I/O settings
in Go applications.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-conpty
  (package
    (name "go-github-com-charmbracelet-x-conpty")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version #:subdir "conpty"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pk7hcsnb36awgr4h4s0mb3sg2p0y7zxb3k2akvxk667n8xpm3mj"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "conpty")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/x/conpty"
      #:unpack-path "github.com/charmbracelet/x"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-github-com-charmbracelet-x-errors))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Windows ConPTY support for Go")
    (description
     "This package provides support for Windows Console Pseudo-Terminal (ConPTY)
in Go applications.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-exp-strings
  (package
    (name "go-github-com-charmbracelet-x-exp-strings")
    (version "0.0.0-20251028133951-21a390f3cede")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version #:subdir "exp/strings"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "088786ak8jsgvssbb5y16z066vfq9f6078sjc7iv766knfr2i0x2"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "exp")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/x/exp/strings"
      #:unpack-path "github.com/charmbracelet/x"))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "String manipulation utilities for Go")
    (description
     "This package provides string manipulation utilities for Go applications.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-x-xpty
  (package
    (name "go-github-com-charmbracelet-x-xpty")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/x")
             (commit (go-version->git-ref version #:subdir "xpty"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nppkr1fci209m69rz1v690vv4y28z6ghfvwxbaxgyfqq1yq2ds7"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "xpty")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/x/xpty"
      #:unpack-path "github.com/charmbracelet/x"))
    (propagated-inputs
     (list go-github-com-charmbracelet-x-conpty
           go-github-com-charmbracelet-x-term
           go-github-com-charmbracelet-x-termios
           go-github-com-creack-pty))
    (home-page "https://github.com/charmbracelet/x")
    (synopsis "Cross-platform pseudo-terminal interface for Go")
    (description
     "This package provides a cross-platform pseudo-terminal (PTY) interface
for Go, supporting both Unix-like systems and Windows ConPTY.")
    (license license:expat)))

(define-public go-github-com-mattn-go-localereader
  (package
    (name "go-github-com-mattn-go-localereader")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattn/go-localereader")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wcgqnpjk0drm7swc0q27j4r5ab63mhz29dgbjdnyn4sw68rqm96"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mattn/go-localereader"
      ;; Tests use Windows-only NewCodePageDecoder function
      #:tests? #f))
    (home-page "https://github.com/mattn/go-localereader")
    (synopsis "Locale-aware reader for Go")
    (description
     "This package provides a locale-aware reader for Go.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-bubbletea
  (package
    (name "go-github-com-charmbracelet-bubbletea")
    (version "1.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/bubbletea")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09bdwp1qsfjkvv7lgwjcvmww88mrz6bb9bani17rs96ag0l4bpaw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/bubbletea"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                ;; Only test main package, skip examples which have extra deps
                (invoke "go" "test" "-v" import-path)))))))
    (propagated-inputs (list go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-github-com-muesli-cancelreader
                             go-github-com-muesli-ansi
                             go-github-com-mattn-go-localereader
                             go-github-com-erikgeiser-coninput
                             go-github-com-charmbracelet-x-term
                             go-github-com-charmbracelet-x-ansi
                             go-github-com-charmbracelet-lipgloss))
    (home-page "https://github.com/charmbracelet/bubbletea")
    (synopsis "TUI framework for Go based on The Elm Architecture")
    (description
     "Package tea provides a framework for building rich terminal user interfaces
based on the paradigms of The Elm Architecture.  It's well-suited for simple and
complex terminal applications, either inline, full-window, or a mix of both.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-bubbles
  (package
    (name "go-github-com-charmbracelet-bubbles")
    (version "0.21.1-0.20250623103423-23b8fd6302d7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/charmbracelet/bubbles")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p1akww623hhj31g1z2xpzw7mbhq6x23qc2fqjfqqn5irmbnp4aj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/charmbracelet/bubbles"))
    (propagated-inputs (list go-github-com-sahilm-fuzzy
                             go-github-com-rivo-uniseg
                             go-github-com-muesli-termenv
                             go-github-com-mattn-go-runewidth
                             go-github-com-lucasb-eyer-go-colorful
                             go-github-com-dustin-go-humanize
                             go-github-com-charmbracelet-x-exp-golden
                             go-github-com-charmbracelet-x-ansi
                             go-github-com-charmbracelet-lipgloss
                             go-github-com-charmbracelet-harmonica
                             go-github-com-charmbracelet-bubbletea
                             go-github-com-aymanbagabas-go-udiff
                             go-github-com-atotto-clipboard
                             go-github-com-makenowjust-heredoc))
    (home-page "https://github.com/charmbracelet/bubbles")
    (synopsis "TUI components for Bubble Tea applications")
    (description
     "Package bubbles provides some components for Bubble Tea applications.  These
components are used in production in Glow, Charm and many other applications.")
    (license license:expat)))

(define-public go-github-com-charmbracelet-huh
  (package
    (name "go-github-com-charmbracelet-huh")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch/lfs)
       (uri (git-reference
             (url "https://github.com/charmbracelet/huh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gbmlw9njfd3ayg3isbj61v9g0f30v772dwp4jv32h6nx14dbqc2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/charmbracelet/huh"
      ;; Test only the main library packages.  The examples/ directory is a
      ;; separate Go module with additional dependencies.
      #:test-subdirs #~'("" "spinner" "accessibility")))
    (propagated-inputs
     (list go-github-com-catppuccin-go
           go-github-com-charmbracelet-bubbles
           go-github-com-charmbracelet-bubbletea
           go-github-com-charmbracelet-lipgloss
           go-github-com-charmbracelet-x-ansi
           go-github-com-charmbracelet-x-cellbuf
           go-github-com-charmbracelet-x-exp-strings
           go-github-com-charmbracelet-x-term
           go-github-com-charmbracelet-x-xpty
           go-github-com-mitchellh-hashstructure))
    (home-page "https://github.com/charmbracelet/huh")
    (synopsis "Terminal forms and prompts library for Go")
    (description
     "This package provides components to build terminal-based forms and
prompts with an interactive and user-friendly interface.  It supports various
input types including text fields, text areas, selects, multi-selects, and
confirm dialogs.")
    (license license:expat)))

(define-public go-github-com-mitchellh-hashstructure
  (package
    (name "go-github-com-mitchellh-hashstructure")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/hashstructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yyr1igvyv7dzjxs9hbwk7qhshwxys0hq59sy2g2a46hjgi311iv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mitchellh/hashstructure/v2"))
    (home-page "https://github.com/mitchellh/hashstructure")
    (synopsis "Go library for creating hash values from Go structures")
    (description
     "This package provides a Go library for creating a unique hash value for
arbitrary values in Go.  It can hash nearly any Go type, including complex
types like structs, arrays, slices, and maps.  This is useful for creating
cache keys, detecting changes, or implementing set operations.")
    (license license:expat)))

(define-public go-github-com-steveyegge-beads
  (package
    (name "go-github-com-steveyegge-beads")
    (version "0.57.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/steveyegge/beads")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z8hikq5aywxxj2kz4n79f6mgyf7cw9wsmn9i4gd7d2llylywc06"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #t
      #:import-path "github.com/steveyegge/beads"
      #:phases
      #~(modify-phases %standard-phases
          ;; No binaries to build, just install source
          (delete 'build)
          (delete 'check))))
    (propagated-inputs (list go-github-com-anthropics-anthropic-sdk-go
                             go-github-com-burntsushi-toml
                             go-github-com-cenkalti-backoff-v4
                             go-github-com-charmbracelet-glamour
                             go-github-com-charmbracelet-lipgloss
                             go-github-com-fsnotify-fsnotify
                             go-github-com-go-sql-driver-mysql
                             go-github-com-muesli-termenv
                             go-github-com-ncruces-go-sqlite3
                             go-github-com-olebedev-when
                             go-github-com-spf13-cobra
                             go-github-com-spf13-viper
                             go-golang-org-x-sys
                             go-golang-org-x-term
                             go-gopkg-in-yaml-v3
                             go-rsc-io-script))
    (home-page "https://github.com/steveyegge/beads")
    (synopsis "Go library for graph-based issue tracking")
    (description
     "This package provides the Go library for Beads, a graph-based issue
tracker for AI coding agents.  It includes the core types, storage interfaces,
and utility functions needed to interact with Beads databases.")
    (license license:expat)))
