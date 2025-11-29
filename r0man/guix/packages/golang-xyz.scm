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
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tetratelabs/wazero")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wz9f9kbbr7315grx4apcszgxpzm8gygx2yi45f7lhgq7lydf9jl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tetratelabs/wazero"
      #:tests? #f))
    (home-page "https://wazero.io/")
    (synopsis "WebAssembly runtime for Go")
    (description
     "wazero is a WebAssembly runtime written in pure Go.  It provides a safe
way to run compiled WebAssembly modules without CGO dependencies.")
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

(define-public go-github-com-steveyegge-beads
  (package
    (name "go-github-com-steveyegge-beads")
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/steveyegge/beads")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00mvpsap1f89fyqk3kzhq8n0wrim6446vqnx30i99g02bbnfzycz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #t
      #:tests? #f  ; Tests require full environment
      #:import-path "github.com/steveyegge/beads"
      #:phases
      #~(modify-phases %standard-phases
          ;; No binaries to build, just install source
          (delete 'build))))
    (propagated-inputs
     (list go-github-com-anthropics-anthropic-sdk-go
           go-github-com-fatih-color
           go-github-com-ncruces-go-sqlite3
           go-github-com-spf13-cobra
           go-github-com-spf13-viper
           go-gopkg-in-natefinch-lumberjack-v2
           go-gopkg-in-yaml-v3
           go-rsc-io-script))
    (home-page "https://github.com/steveyegge/beads")
    (synopsis "Go library for graph-based issue tracking")
    (description
     "This package provides the Go library for Beads, a graph-based issue
tracker for AI coding agents.  It includes the core types, storage interfaces,
and utility functions needed to interact with Beads databases.")
    (license license:expat)))
