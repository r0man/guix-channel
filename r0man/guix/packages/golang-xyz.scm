(define-module (r0man guix packages golang-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
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
