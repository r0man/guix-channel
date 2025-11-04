(define-module (r0man guix packages golang-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (r0man guix packages golang-maths))

(define-public go-github-com-google-uuid
  (package
    (name "go-github-com-google-uuid")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/uuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "131d01minir79dq6d4jq55018343yidl5cs2bfhynx1klnr7ssam"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/uuid"))
    (home-page "https://github.com/google/uuid")
    (synopsis "Go package for UUIDs")
    (description
     "This package provides UUID generation and parsing for Go.")
    (license license:bsd-3)))

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

(define-public go-github-com-ncruces-go-sqlite3
  (package
    (name "go-github-com-ncruces-go-sqlite3")
    (version "0.29.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncruces/go-sqlite3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lf770zxsm2fafr53nrxxhshyqhpxpq0nmwxksvmkfkdlzdb3gw9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ncruces/go-sqlite3"
      #:tests? #f))
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

(define-public go-gopkg-in-natefinch-lumberjack-v2
  (package
    (name "go-gopkg-in-natefinch-lumberjack-v2")
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/natefinch/lumberjack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1jylmjfrx79c6vkx8sqlyrb54krqgjrgj138w8cjw6v17i2dd98r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/natefinch/lumberjack.v2"))
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/natefinch/lumberjack")
    (synopsis "Rolling logger for Go")
    (description
     "Lumberjack is a Go package for writing logs to rolling files.
It provides automatic log rotation based on file size, age, and count,
with optional compression of rotated files.")
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

(define-public go-github-com-tidwall-match
  (package
    (name "go-github-com-tidwall-match")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tidwall/match")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n25md63xr5m66r6zc77n6fgcpv2ljrlk92ivp9hvp8xya22as9k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tidwall/match"))
    (home-page "https://github.com/tidwall/match")
    (synopsis "Simple string pattern matcher for Golang")
    (description
     "Package match provides a simple pattern matcher with unicode support.")
    (license license:expat)))

(define-public go-github-com-tidwall-pretty
  (package
    (name "go-github-com-tidwall-pretty")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tidwall/pretty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0prj9vpjgrca70rvx40kkl566yf9lw4fsbcmszwamwl364696jsb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tidwall/pretty"))
    (home-page "https://github.com/tidwall/pretty")
    (synopsis "JSON beautifier and compactor for Golang")
    (description
     "This package provides fast methods for formatting JSON for human
readability, or to compact JSON for smaller payloads.")
    (license license:expat)))

(define-public go-github-com-tidwall-gjson
  (package
    (name "go-github-com-tidwall-gjson")
    (version "1.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tidwall/gjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gcjzbs5in4kics39d2v3j2v9gvfxkdgp0bdgbfmcsa5arqgq7g5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tidwall/gjson"))
    (propagated-inputs
     (list go-github-com-tidwall-match
           go-github-com-tidwall-pretty))
    (home-page "https://github.com/tidwall/gjson")
    (synopsis "JSON parser for Golang")
    (description
     "This package provides a fast and simple way to get values from a JSON
document.  It has features such as one line retrieval, dot notation paths,
iteration, and parsing JSON lines.")
    (license license:expat)))

(define-public go-github-com-tidwall-sjson
  (package
    (name "go-github-com-tidwall-sjson")
    (version "1.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tidwall/sjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16yaikpxiwqz00zxa70w17k2k52nr06svand88sv2br6b6i8v09r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tidwall/sjson"))
    (propagated-inputs
     (list go-github-com-tidwall-gjson
           go-github-com-tidwall-pretty))
    (home-page "https://github.com/tidwall/sjson")
    (synopsis "Quick value JSON values setting in Golang")
    (description
     "This package provides a fast and simple way to set a value in a JSON
document.")
    (license license:expat)))

(define-public go-rsc-io-script
  (package
    (name "go-rsc-io-script")
    (version "0.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsc/script")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05yl5nqhcjhp4sjdh7a9m9s6w4lm4qhn4bhi7v6hhsbfn348jxfh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "rsc.io/script"))
    (propagated-inputs (list go-golang-org-x-tools))
    (home-page "https://rsc.io/script")
    (synopsis "Small scripting language for Go")
    (description
     "Package script implements a small, customizable, platform-agnostic
scripting language.")
    (license license:bsd-3)))
