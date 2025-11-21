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
  #:use-module (r0man guix packages golang-xyz)
  #:use-module (r0man guix packages task-management))

(define-public vibecoder
  (let ((revision "0")
        (commit "4ae43b6a4281704ee072b7c0b69b20e3d53837ee"))
    (package
      (name "vibecoder")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/steveyegge/vc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ii5vc23kv2s3iawhyyzyljrbll4i6s7156h048zyzwbj1idyvlf"))))
      (build-system go-build-system)
      (arguments
       (list
        #:install-source? #f
        #:tests? #f  ; Tests require ANTHROPIC_API_KEY and full environment
        #:import-path "github.com/steveyegge/vc/cmd/vc"
        #:unpack-path "github.com/steveyegge/vc"
        #:embed-files #~(list ".*\\.wasm")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'remove-replace-directive
              (lambda _
                ;; Remove the replace directive that points to a local beads path
                (substitute* "src/github.com/steveyegge/vc/go.mod"
                  (("replace github.com/steveyegge/beads.*") ""))))
            (add-before 'build 'set-home
              (lambda _
                (setenv "HOME" "/tmp"))))))
    (native-inputs (list git))
    (propagated-inputs (list go-github-com-steveyegge-beads
                             go-github-com-chzyer-readline
                             go-github-com-fatih-color
                             go-github-com-google-uuid
                             go-github-com-ncruces-go-sqlite3
                             go-github-com-spf13-cobra
                             go-github-com-stretchr-testify
                             go-golang-org-x-mod
                             go-golang-org-x-sync
                             go-golang-org-x-time
                             go-gopkg-in-yaml-v3))
    (home-page "https://github.com/steveyegge/vc")
    (synopsis "AI-orchestrated coding agent colony")
    (description
     "@command{vc} (VibeCoder v2) orchestrates multiple coding agents
(Amp, Claude Code, etc.) to work on small, well-defined tasks, guided
by AI supervision.  This keeps agents focused, improves quality, and
minimizes context window costs.  The system uses an AI-supervised
issue workflow where work is tracked in a SQLite issue tracker with
dependency awareness, workflows can be interrupted and resumed, and
quality gates validate the results.  Built on lessons learned from a
350k LOC TypeScript prototype.")
      (license license:expat))))
