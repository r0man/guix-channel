(define-module (r0man guix packages task-management)
    #:use-module ((guix licenses) #:prefix license:)
    #:use-module (gnu packages golang)
    #:use-module (gnu packages golang-build)
    #:use-module (gnu packages golang-check)
    #:use-module (gnu packages golang-web)
    #:use-module (gnu packages golang-xyz)
    #:use-module (gnu packages tmux)
    #:use-module (gnu packages version-control)
    #:use-module (gnu packages)
    #:use-module (guix build-system go)
    #:use-module (guix gexp)
    #:use-module (guix git-download)
    #:use-module (guix packages)
    #:use-module (r0man guix packages golang-xyz))

(define-public beads-next
  (let ((commit "ff67b88ea9bc6fe699b24c26a5c46a69c05b6d38")
        (revision "2"))
    (package
      (name "beads-next")
      (version (git-version "0.47.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/steveyegge/beads")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0nmdbg1sk8yxk73h9b47yjrql8piiz55ygfbg04hwa98y3g5cw9n"))))
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
                    (invoke "go"
                            "test"
                            "-v"
                            "-run"
                            (string-append "^Test(Parse|ValidationResults"
                                           "|VersionCommand|Truncate"
                                           "|GitRevParse)")
                            ".")))))
            (add-after 'unpack 'delete-broken-test
              (lambda _
                (delete-file
                 "src/github.com/steveyegge/beads/cmd/bd/integrity_content_test.go")))
            (add-after 'unpack 'fix-embedded-symlinks
              (lambda _
                (use-modules (ice-9 ftw))
                ;; Replace symlinked files with actual copies
                ;; to work around Go embed limitation with Guix store
                (define (copy-symlink-targets dir)
                  (when (file-exists? dir)
                    (for-each (lambda (file)
                                (let ((path (string-append dir "/" file)))
                                  (when (symbolic-link? path)
                                    (let ((target (readlink path)))
                                      (delete-file path)
                                      (copy-file target path)))))
                              (scandir dir
                                       (lambda (f)
                                         (not (member f
                                                      '("." ".."))))))))
                ;; Fix sqlite3 WASM files
                (let ((sqlite-dir "src/github.com/ncruces/go-sqlite3"))
                  (for-each (lambda (wasm-file)
                              (when (and (file-exists? wasm-file)
                                         (symbolic-link? wasm-file))
                                (let ((target (readlink wasm-file)))
                                  (delete-file wasm-file)
                                  (copy-file target wasm-file))))
                            (list (string-append sqlite-dir
                                                 "/embed/sqlite3.wasm")
                                  (string-append sqlite-dir "/util/sql3util"
                                                 "/wasm/sql3parse_table.wasm"))))
                ;; Fix chroma lexer and style embedded files
                (copy-symlink-targets
                 "src/github.com/alecthomas/chroma/v2/lexers/embedded")
                (copy-symlink-targets
                 "src/github.com/alecthomas/chroma/v2/styles")))
            (add-before 'build 'set-home
              (lambda _
                (setenv "HOME" "/tmp")))
            (add-after 'install 'install-completions
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bd (string-append out "/bin/bd"))
                       (bash-dir (string-append out "/etc/bash_completion.d"))
                       (zsh-dir (string-append out "/share/zsh/site-functions"))
                       (fish-dir (string-append out
                                  "/share/fish/vendor_completions.d")))
                  (mkdir-p bash-dir)
                  (mkdir-p zsh-dir)
                  (mkdir-p fish-dir)
                  (with-output-to-file (string-append bash-dir "/bd")
                    (lambda ()
                      (system* bd "completion" "bash")))
                  (with-output-to-file (string-append zsh-dir "/_bd")
                    (lambda ()
                      (system* bd "completion" "zsh")))
                  (with-output-to-file (string-append fish-dir "/bd.fish")
                    (lambda ()
                      (system* bd "completion" "fish")))))))))
      (native-inputs (list git
                           go-github-com-anthropics-anthropic-sdk-go
                           go-github-com-charmbracelet-glamour
                           go-github-com-charmbracelet-huh
                           go-github-com-charmbracelet-lipgloss
                           go-github-com-fatih-color
                           go-github-com-gofrs-flock
                           go-github-com-ncruces-go-sqlite3
                           go-github-com-olebedev-when
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
      (license license:expat))))

(define-public gastown-next
  (let ((commit "6b8480c4839fc55c79a240589983bdf9eaa571a8")
        (revision "1"))
    (package
      (name "gastown-next")
      (version (git-version "0.2.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/steveyegge/gastown")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qjxkhppl3902q8rjh7ijfbk02m607jy8gxh68jkiyfgay3w388y"))))
      (build-system go-build-system)
      (arguments
       (list
        #:install-source? #f
        #:import-path "github.com/steveyegge/gastown/cmd/gt"
        #:unpack-path "github.com/steveyegge/gastown"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'run-go-generate
              (lambda* (#:key import-path #:allow-other-keys)
                ;; Run go generate to provision embedded formula files.
                (with-directory-excursion (string-append "src/"
                                                         (dirname (dirname
                                                                   import-path))
                                                         "/internal/formula")
                  (invoke "go" "generate"))))
            (add-after 'run-go-generate 'remove-beads-directory
              (lambda* (#:key import-path #:allow-other-keys)
                ;; Remove .beads directory so integration tests skip gracefully.
                ;; The directory contains only JSONL without an initialized
                ;; SQLite database, which would cause TestIntegration to fail.
                (let ((beads-dir (string-append "src/"
                                                (dirname (dirname import-path))
                                                "/.beads")))
                  (when (file-exists? beads-dir)
                    (delete-file-recursively beads-dir)))))
            (add-after 'remove-beads-directory 'fix-embedded-symlinks
              (lambda _
                (use-modules (ice-9 ftw))
                ;; Replace symlinked files with actual copies to work around
                ;; Go embed limitation with Guix store.
                (define (copy-symlink-targets dir)
                  (when (file-exists? dir)
                    (for-each (lambda (file)
                                (let ((path (string-append dir "/" file)))
                                  (when (symbolic-link? path)
                                    (let ((target (readlink path)))
                                      (delete-file path)
                                      (copy-file target path)))))
                              (scandir dir
                                       (lambda (f)
                                         (not (member f
                                                      '("." ".."))))))))
                ;; Fix chroma lexer and style embedded files.
                (copy-symlink-targets
                 "src/github.com/alecthomas/chroma/v2/lexers/embedded")
                (copy-symlink-targets
                 "src/github.com/alecthomas/chroma/v2/styles")))
            (delete 'check)
            (add-before 'build 'set-home
              (lambda _
                (setenv "HOME" "/tmp")))
            (add-after 'install 'install-completions
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (gt (string-append out "/bin/gt"))
                       (bash-dir (string-append out "/etc/bash_completion.d"))
                       (zsh-dir (string-append out "/share/zsh/site-functions"))
                       (fish-dir (string-append out
                                  "/share/fish/vendor_completions.d")))
                  (mkdir-p bash-dir)
                  (mkdir-p zsh-dir)
                  (mkdir-p fish-dir)
                  (with-output-to-file (string-append bash-dir "/gt")
                    (lambda ()
                      (system* gt "completion" "bash")))
                  (with-output-to-file (string-append zsh-dir "/_gt")
                    (lambda ()
                      (system* gt "completion" "zsh")))
                  (with-output-to-file (string-append fish-dir "/gt.fish")
                    (lambda ()
                      (system* gt "completion" "fish")))))))))
      (native-inputs (list git
                           go-github-com-burntsushi-toml
                           go-github-com-charmbracelet-bubbles
                           go-github-com-charmbracelet-bubbletea
                           go-github-com-charmbracelet-glamour
                           go-github-com-charmbracelet-lipgloss
                           go-github-com-gofrs-flock
                           go-github-com-google-uuid
                           go-github-com-spf13-cobra
                           go-golang-org-x-term
                           go-golang-org-x-text))
      (inputs (list beads-next tmux))
      (home-page "https://github.com/steveyegge/gastown")
      (synopsis "Multi-agent orchestrator for Claude Code")
      (description
       "@command{gt} (Gastown) is a multi-agent orchestrator for Claude Code
that coordinates multiple AI agents working on software development tasks.
It uses a git-backed issue tracker called Beads to maintain work state,
ensuring tasks survive crashes and agent restarts.  Agents are organized
into roles (Polecats for workers, Witness for monitoring, Refinery for
code review, Mayor for cross-project coordination) within containerized
project spaces called Rigs.")
      (license license:expat))))
