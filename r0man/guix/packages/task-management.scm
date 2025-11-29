(define-module (r0man guix packages task-management)
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
    (version "0.26.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/steveyegge/beads")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x0ijpprzfa2s68ad95als1pg3jgj3f41ya3v2z85gjd3h0vlh1f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/steveyegge/beads/cmd/bd"
      #:unpack-path "github.com/steveyegge/beads"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                ;; Run only unit tests, skipping CLI integration tests that fail
                ;; in the sandboxed build environment
                (with-directory-excursion (string-append "src/" import-path)
                  ;; Run tests that don't require full environment
                  (invoke "go" "test" "-v" "-run"
                          "^Test(Parse|ValidationResults|VersionCommand|Truncate|GitRevParse)"
                          ".")))))
          (add-after 'unpack 'delete-broken-test
            (lambda _
              (delete-file "src/github.com/steveyegge/beads/cmd/bd/integrity_content_test.go")))
          (add-after 'unpack 'fix-wasm-symlinks
            (lambda _
              ;; Replace symlinked WASM files with actual copies
              ;; to work around Go embed limitation with Guix store
              (let ((sqlite-dir "src/github.com/ncruces/go-sqlite3"))
                (for-each (lambda (wasm-file)
                            (when (and (file-exists? wasm-file)
                                       (symbolic-link? wasm-file))
                              (let ((target (readlink wasm-file)))
                                (delete-file wasm-file)
                                (copy-file target wasm-file))))
                          (list (string-append sqlite-dir
                                               "/embed/sqlite3.wasm")
                                (string-append sqlite-dir
                                               "/util/sql3util/wasm/sql3parse_table.wasm"))))))
          (add-before 'build 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs (list git))
    (propagated-inputs (list go-github-com-anthropics-anthropic-sdk-go
                             go-github-com-fatih-color
                             go-github-com-ncruces-go-sqlite3
                             go-github-com-spf13-cobra
                             go-github-com-spf13-viper
                             go-gopkg-in-natefinch-lumberjack-v2
                             go-gopkg-in-yaml-v3
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
