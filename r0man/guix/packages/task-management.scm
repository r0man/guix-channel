(define-module (r0man guix packages task-management)
    #:use-module ((guix licenses) #:prefix license:)
    #:use-module (gnu packages golang)
    #:use-module (gnu packages golang-build)
    #:use-module (gnu packages golang-check)
    #:use-module (gnu packages golang-web)
    #:use-module ((gnu packages golang-xyz)
                   #:hide (go-github-com-charmbracelet-bubbles
                           go-github-com-charmbracelet-bubbletea))
    #:use-module (gnu packages icu4c)
    #:use-module (gnu packages tmux)
    #:use-module (gnu packages version-control)
    #:use-module (gnu packages)
    #:use-module (guix build-system go)
    #:use-module (guix gexp)
    #:use-module (guix git-download)
    #:use-module (guix packages)
    #:use-module (r0man guix packages golang-dolthub)
    #:use-module (r0man guix packages golang-web)
    #:use-module (r0man guix packages golang-xyz))

(define-public beads-next
  (let ((commit "14df21b6296c0d7501376e79716d944d0d8995c9")
        (revision "67"))
    (package
      (name "beads-next")
      (version (git-version "0.59.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/steveyegge/beads")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ibk7gzg9yv87f1rigd49b18mng61qxvd0jqw5j6zilss4yl97if"))))
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
                  ;; Run only the setup tests which don't depend on
                  ;; testcontainers-go (Docker).  The other test packages
                  ;; import internal/testutil which pulls in testcontainers-go
                  ;; for Dolt container management, unavailable in the sandbox.
                  (invoke "go" "test" "-v"
                          "github.com/steveyegge/beads/cmd/bd/setup"))))
            (add-after 'unpack 'fix-embedded-symlinks
              (lambda _
                (use-modules (ice-9 ftw))
                ;; Replace symlinked files with actual copies
                ;; to work around Go embed limitation with Guix store.
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
                      icu4c
                      go-github-com-anthropics-anthropic-sdk-go
                      go-github-com-burntsushi-toml
                      go-github-com-cenkalti-backoff-v4
                      go-github-com-cenkalti-backoff-v5
                      go-github-com-charmbracelet-glamour
                      go-github-com-charmbracelet-huh
                      go-github-com-charmbracelet-lipgloss
                      go-github-com-dolthub-driver
                      go-github-com-fsnotify-fsnotify
                      go-github-com-go-sql-driver-mysql
                      go-github-com-muesli-termenv
                      go-github-com-olebedev-when
                      go-github-com-spf13-cobra
                      go-github-com-spf13-viper
                      go-github-com-stretchr-testify
                      go-go-opentelemetry-io-otel
                      go-go-opentelemetry-io-otel-exporters-otlp-otlpmetric-otlpmetrichttp
                      go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric
                      go-go-opentelemetry-io-otel-exporters-stdout-stdouttrace
                      go-go-opentelemetry-io-otel-sdk
                      go-go-opentelemetry-io-otel-sdk-metric
                      go-go-opentelemetry-io-proto-otlp
                      go-golang-org-x-sync
                      go-golang-org-x-sys
                      go-golang-org-x-term
                      go-google-golang-org-grpc
                      go-google-golang-org-protobuf
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
  (let ((commit "bf260dc7b14c5c58e33f2433f2555e7fae2ab503")
        (revision "5839"))
    (package
      (name "gastown-next")
      (version (git-version "0.11.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/steveyegge/gastown")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11gmr0brvdr9mgq7sbxlxwb4y3j0nq9hlq662mq9fn89dmsbyxpa"))))
      (build-system go-build-system)
      (arguments
       (list
        #:install-source? #f
        #:import-path "github.com/steveyegge/gastown/cmd/gt"
        #:unpack-path "github.com/steveyegge/gastown"
        #:build-flags
        #~(list (string-append "-ldflags="
                 "-X github.com/steveyegge/gastown/internal/cmd.BuiltProperly=1"
                 " -X github.com/steveyegge/gastown/internal/cmd.Build=v"
                 #$(package-version this-package)))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'remove-beads-directory
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
                      go-github-com-dolthub-dolt-go
                      go-github-com-dolthub-driver
                      go-github-com-go-rod-rod
                      go-github-com-go-sql-driver-mysql
                      go-github-com-gofrs-flock
                      go-github-com-google-uuid
                      go-github-com-muesli-termenv
                      go-github-com-spf13-cobra
                      go-github-com-steveyegge-beads
                      go-go-opentelemetry-io-otel
                      go-go-opentelemetry-io-otel-exporters-otlp-otlplog-otlploghttp
                      go-go-opentelemetry-io-otel-exporters-otlp-otlpmetric-otlpmetrichttp
                      go-go-opentelemetry-io-otel-log
                      go-go-opentelemetry-io-otel-metric
                      go-go-opentelemetry-io-otel-sdk
                      go-go-opentelemetry-io-otel-sdk-log
                      go-go-opentelemetry-io-otel-sdk-metric
                      go-go-opentelemetry-io-proto-otlp
                      go-github-com-cenkalti-backoff-v5
                      go-golang-org-x-sys
                      go-golang-org-x-term
                      go-golang-org-x-text
                      go-golang-org-x-time
                      go-gopkg-in-natefinch-lumberjack-v2
                      go-gopkg-in-yaml-v3))
      (propagated-inputs (list beads-next dolt tmux))
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
