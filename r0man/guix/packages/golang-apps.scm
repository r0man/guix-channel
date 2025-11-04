(define-module (r0man guix packages golang-apps)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (r0man guix packages golang-web)
  #:use-module (r0man guix packages golang-xyz))

(define-public beads
  (package
    (name "beads")
    (version "0.21.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/steveyegge/beads")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nybx5rm0lxr86dzam5rf67rzg19l0rif57cl35wkp5l5qlfs1bh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/steveyegge/beads/cmd/bd"
      #:unpack-path "github.com/steveyegge/beads"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list git))
    (propagated-inputs
     (list go-github-com-anthropics-anthropic-sdk-go
           go-github-com-fatih-color
           go-github-com-spf13-cobra
           go-github-com-spf13-viper
           go-gopkg-in-natefinch-lumberjack-v2
           go-modernc-org-sqlite
           go-rsc-io-script))
    (home-page "https://github.com/steveyegge/beads")
    (synopsis "Graph-based issue tracker for AI coding agents")
    (description
     "@command{bd} (Beads) is a lightweight memory system for coding
agents, using a graph-based issue tracker.  Four kinds of dependencies
work to chain issues together like beads, making them easy for agents
to follow for long distances and reliably perform complex task streams
in the right order.  It uses SQLite for fast local operations and
JSONL files stored in git for distributed synchronization across
machines.")
    (license license:expat)))
