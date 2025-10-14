(define-module (r0man guix packages golang-apps)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public bd
  (package
    (name "bd")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/steveyegge/beads")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nvan2lwcw8zhhpbr5dpz8xvpf4c02j0vgf14xbgm0lvamwa2hgp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/steveyegge/beads/cmd/bd"
      #:unpack-path "github.com/steveyegge/beads"))
    (native-inputs
     (list go-github-com-fatih-color
           go-github-com-mattn-go-sqlite3
           go-github-com-spf13-cobra
           go-github-com-spf13-viper))
    (home-page "https://github.com/steveyegge/beads")
    (synopsis "Graph-based issue tracker for AI coding agents")
    (description
     "@command{bd} (Beads) is a lightweight memory system for coding agents,
using a graph-based issue tracker.  Four kinds of dependencies work to chain
issues together like beads, making them easy for agents to follow for long
distances and reliably perform complex task streams in the right order.  It
uses SQLite for fast local operations and JSONL files stored in git for
distributed synchronization across machines.")
    (license license:expat)))
