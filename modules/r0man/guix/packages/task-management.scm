(define-module (r0man guix packages task-management)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module ((gnu packages golang-web)
                #:hide (go-github-com-anthropics-anthropic-sdk-go
                        go-github-com-danielgtaylor-huma-v2
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-otel-exporters-stdout-stdouttrace
                        go-go-opentelemetry-io-otel-log
                        go-go-opentelemetry-io-otel-metric
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-sdk-log
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-proto-otlp))
  #:use-module ((gnu packages golang-xyz)
                #:hide (go-github-com-charmbracelet-bubbles
                        go-github-com-charmbracelet-bubbletea
                        go-github-com-charmbracelet-colorprofile
                        go-github-com-charmbracelet-x-ansi
                        go-github-com-charmbracelet-x-cellbuf
                        go-github-com-charmbracelet-x-term
                        go-github-com-charmbracelet-x-windows))
  #:use-module ((gnu packages base)
                #:select (coreutils grep))
  #:use-module ((gnu packages bash)
                #:select (bash-minimal))
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages python)
  #:use-module ((gnu packages python-xyz)
                #:select (python-pyyaml))
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages web)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages)
  #:use-module (guix build-system go)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (r0man guix packages golang-charm)
  #:use-module (r0man guix packages golang-dolthub)
  #:use-module (r0man guix packages golang-web)
  #:use-module (r0man guix packages golang-xyz))

(define-public beads-next
  (package
    (name "beads-next")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gastownhall/beads")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kb3n6chz0x3ap77kf41grvyajc97kdrrfk5lb7h07cshp7pa9hx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.26
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
               "src/github.com/alecthomas/chroma/v2/styles")
              ;; Fix dolt embedded files (AGENT.md, weight maps).
              (copy-symlink-targets (string-append
                                     "src/github.com/dolthub/dolt/go"
                                     "/libraries/doltcore/doltdb"))
              (copy-symlink-targets (string-append
                                     "src/github.com/dolthub/go-mysql-server"
                                     "/sql/encodings"))))
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
                    ;; Updated charmbracelet/x packages must appear before
                    ;; packages that propagate older versions, so they win
                    ;; collision resolution in setup-go-environment.
                    go-github-com-charmbracelet-colorprofile
                    go-github-com-charmbracelet-ultraviolet
                    go-github-com-charmbracelet-x-ansi
                    go-github-com-charmbracelet-x-cellbuf
                    go-github-com-charmbracelet-x-term
                    go-github-com-charmbracelet-x-windows
                    go-github-com-anthropics-anthropic-sdk-go
                    go-github-com-burntsushi-toml
                    go-github-com-cenkalti-backoff-v4
                    go-github-com-cenkalti-backoff-v5
                    go-charm-land-glamour-v2
                    go-charm-land-huh-v2
                    go-charm-land-lipgloss-v2
                    go-github-com-charmbracelet-lipgloss
                    go-github-com-dolthub-driver-v2
                    go-github-com-dolthub-eventkit
                    ;; Transitive dolt CLI dependencies needed for
                    ;; compilation of the full dolt source tree.
                    go-github-com-abiosoft-readline
                    go-github-com-andreyvit-diff
                    go-github-com-dolthub-ishell
                    go-github-com-flynn-archive-go-shlex
                    go-github-com-google-go-github-v57
                    go-github-com-google-shlex
                    go-github-com-pkg-profile
                    go-github-com-skratchdot-open-golang
                    go-github-com-tealeg-xlsx
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
                    go-github-com-johanneskaufmann-dom
                    go-github-com-johanneskaufmann-html-to-markdown-v2
                    go-gopkg-in-yaml-v3
                    go-rsc-io-script))
    (home-page "https://github.com/gastownhall/beads")
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

(define-public go-github-com-steveyegge-beads
  (let ((commit "3a7a2e852a739f43e3c6c58ee36693dea8bac693")
        (revision "0"))
    (package
      (name "go-github-com-steveyegge-beads")
      (version (git-version "1.0.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gastownhall/beads")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12gmp0ddmdbs6h7ysqpx79n721854ir23pjia27pcr0raj7hfspb"))))
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
                               go-github-com-charmbracelet-colorprofile
                               go-charm-land-glamour-v2
                               go-charm-land-huh-v2
                               go-github-com-charmbracelet-lipgloss
                               go-github-com-dolthub-driver
                               go-github-com-fsnotify-fsnotify
                               go-github-com-go-sql-driver-mysql
                               go-github-com-muesli-termenv
                               go-github-com-olebedev-when
                               go-github-com-spf13-cobra
                               go-github-com-spf13-viper
                               go-golang-org-x-sys
                               go-golang-org-x-term
                               go-gopkg-in-yaml-v3
                               go-rsc-io-script))
      (home-page "https://github.com/gastownhall/beads")
      (synopsis "Go library for graph-based issue tracking")
      (description
       "This package provides the Go library for Beads, a graph-based issue
tracker for AI coding agents.  It includes the core types, storage interfaces,
and utility functions needed to interact with Beads databases.")
      (license license:expat))))

(define-public go-github-com-steveyegge-beads-next
  ;; Library variant of beads-next, required by gascity-next 1.3.5+.  Kept
  ;; separate from go-github-com-steveyegge-beads (pinned at 1.0.4), which is
  ;; still used by gastown-next via the dolt v1 release line.
  (package
    (inherit go-github-com-steveyegge-beads)
    (name "go-github-com-steveyegge-beads-next")
    (version (package-version beads-next))
    (source (package-source beads-next))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs go-github-com-steveyegge-beads)
       (delete "go-github-com-dolthub-driver")
       (prepend go-github-com-dolthub-driver-v2
                go-github-com-dolthub-eventkit)))))

(define-public gastown-next
  (package
    (name "gastown-next")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gastownhall/gastown")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qmz2iad1yawlz906h34k8idznj6m5d9ggkcl5d9waidrwy2jysk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.26
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
               "src/github.com/alecthomas/chroma/v2/styles")
              ;; Fix beads migrations embedded by the beads library.
              (copy-symlink-targets (string-append
                                     "src/github.com/steveyegge/beads"
                                     "/internal/storage/schema/migrations"))
              (copy-symlink-targets (string-append
                                     "src/github.com/steveyegge/beads"
                                     "/internal/storage/schema/migrations/ignored"))
              ;; Fix dolt embedded files (AGENT.md, weight maps).
              (copy-symlink-targets (string-append
                                     "src/github.com/dolthub/dolt/go"
                                     "/libraries/doltcore/doltdb"))
              (copy-symlink-targets (string-append
                                     "src/github.com/dolthub/go-mysql-server"
                                     "/sql/encodings"))))
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
                    icu4c
                    ;; Updated charmbracelet/x packages must appear before
                    ;; packages that propagate older versions, so they win
                    ;; collision resolution in setup-go-environment.
                    go-github-com-charmbracelet-colorprofile
                    go-github-com-charmbracelet-x-ansi
                    go-github-com-charmbracelet-x-cellbuf
                    go-github-com-charmbracelet-x-term
                    go-github-com-charmbracelet-x-windows
                    go-github-com-burntsushi-toml
                    go-github-com-charmbracelet-bubbles
                    go-github-com-charmbracelet-bubbletea
                    go-github-com-charmbracelet-glamour
                    go-github-com-charmbracelet-lipgloss
                    go-github-com-dolthub-dolt-go
                    go-github-com-dolthub-driver
                    ;; Transitive dolt CLI dependencies needed for
                    ;; compilation of the full dolt source tree.
                    go-github-com-abiosoft-readline
                    go-github-com-andreyvit-diff
                    go-github-com-dolthub-ishell
                    go-github-com-flynn-archive-go-shlex
                    go-github-com-google-go-github-v57
                    go-github-com-google-shlex
                    go-github-com-pkg-profile
                    go-github-com-skratchdot-open-golang
                    go-github-com-tealeg-xlsx
                    go-github-com-fsnotify-fsnotify
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
    (home-page "https://github.com/gastownhall/gastown")
    (synopsis "Multi-agent orchestrator for Claude Code")
    (description
     "@command{gt} (Gastown) is a multi-agent orchestrator for Claude Code
that coordinates multiple AI agents working on software development tasks.
It uses a git-backed issue tracker called Beads to maintain work state,
ensuring tasks survive crashes and agent restarts.  Agents are organized
into roles (Polecats for workers, Witness for monitoring, Refinery for
code review, Mayor for cross-project coordination) within containerized
project spaces called Rigs.")
    (license license:expat)))

(define-public go-github-com-gastownhall-gascity-packs
  (package
    (name "go-github-com-gastownhall-gascity-packs")
    (version "0.3.1-0.20260617013242-33d3a430a67d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gastownhall/gascity-packs")
             (commit "33d3a430a67d1782ad364556cb566bdb01d0afe3")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dlj2plrnsxqn974qjfbj89pmwslzf6pkp1fk06zg1rx9bvd3lq8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #t
      #:import-path "github.com/gastownhall/gascity-packs"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check))))
    (home-page "https://github.com/gastownhall/gascity-packs")
    (synopsis "Embedded pack content for Gas City")
    (description
     "This package exposes the Gas City registry pack content as embedded Go
filesystems, so the @command{gc} binary can depend on released pack bytes
through the Go module system instead of vendoring checked-in copies.")
    (license license:expat)))

(define-public python-for-gascity
  ;; Gas City's gate runner (internal/convergence/condition.go) executes
  ;; check scripts under a hard environment whitelist: GUIX_PYTHONPATH never
  ;; reaches them, so the profile's python cannot discover propagated Python
  ;; libraries there.  This wrapper bakes the search path for the packages
  ;; below into bin/python3 itself, so e.g. `python3 -c "import yaml"` works
  ;; even under `env -i PATH=<profile>/bin`.  Add future gate-script
  ;; dependencies to this list.
  (let ((python-packages (list python-pyyaml)))
    (package
      (name "python-for-gascity")
      (version (package-version python))
      (source #f)
      (build-system trivial-build-system)
      (arguments
       (list
        #:builder
        #~(begin
            (use-modules (ice-9 ftw))
            (let* ((out #$output)
                   (bin (string-append out "/bin"))
                   (python #$python)
                   (pyversion #$(version-major+minor
                                 (package-version python)))
                   (interpreter (string-append "python" pyversion))
                   (site (string-append "/lib/python" pyversion
                                        "/site-packages"))
                   (pythonpath (string-join
                                (map (lambda (p) (string-append p site))
                                     (list #$@python-packages))
                                ":"))
                   (wrapper (string-append bin "/" interpreter)))
              (mkdir out)
              (mkdir bin)
              (call-with-output-file wrapper
                (lambda (port)
                  (display (string-append
                            "#!" #$bash-minimal "/bin/bash\n"
                            "export GUIX_PYTHONPATH=\"" pythonpath
                            "${GUIX_PYTHONPATH:+:$GUIX_PYTHONPATH}\"\n"
                            "exec \"" python "/bin/" interpreter
                            "\" \"$@\"\n")
                           port)))
              (chmod wrapper #o555)
              (symlink interpreter (string-append bin "/python3"))
              ;; Pass the rest of python's bin/ through untouched.
              (for-each
               (lambda (file)
                 (let ((target (string-append bin "/" file)))
                   (unless (file-exists? target)
                     (symlink (string-append python "/bin/" file)
                              target))))
               (scandir (string-append python "/bin")
                        (lambda (file)
                          (not (member file (list "." ".."))))))))))
      (home-page "https://github.com/gastownhall/gascity")
      (synopsis "Python with baked-in libraries for Gas City gate scripts")
      (description
       "This package wraps @code{python} so that @file{bin/python3} sets
@env{GUIX_PYTHONPATH} to the site-packages of a fixed list of Python
libraries (currently @code{python-pyyaml}) before exec'ing the real
interpreter.  Gas City's convergence gates run check scripts under a hard
environment whitelist that strips @env{GUIX_PYTHONPATH}, so a plain profile
python cannot see propagated libraries there; this wrapper makes the pack's
build-artifact and verdict validators work in that environment with no
per-city state.")
      (license (package-license python)))))

(define-public gascity-next
  (package
    (name "gascity-next")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gastownhall/gascity")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "007pzqh9n3gxh7gjl2zvg38qvriib1hxw80iwd39m2mfxgdvl4j2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.26
      #:install-source? #f
      #:import-path "github.com/gastownhall/gascity/cmd/gc"
      #:unpack-path "github.com/gastownhall/gascity"
      #:build-flags
      #~(list (string-append "-ldflags=" "-s -w" " -X main.version=v"
                             #$(package-version this-package)))
      ;; gc embeds files from dependencies that setup-go symlinks into GOPATH,
      ;; but go:embed refuses symlinked files.  The standard 'fix-embed-files
      ;; phase materialises every symlink whose base name matches one of these
      ;; patterns (recursively under src/), which covers:
      ;;   - dolt doltdb        //go:embed AGENT.md          (.md)
      ;;   - go-mysql-server    //go:embed *_Weights.bin     (.bin)
      ;;   - beads migrations   //go:embed migrations/*.up.sql (.sql)
      ;;   - gascity-packs      //go:embed all:gastown all:gascity
      ;;                        (.md .toml .sh .py .yaml .txt .tmpl .json)
      #:embed-files
      #~(list ".*\\.md"
              ".*\\.bin"
              ".*\\.sql"
              ".*\\.toml"
              ".*\\.sh"
              ".*\\.py"
              ".*\\.yaml"
              ".*\\.txt"
              ".*\\.tmpl"
              ".*\\.json")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          (add-after 'unpack 'enable-go122-servemux-patterns
            ;; The GOPATH-mode build ignores go.mod, so the toolchain bakes
            ;; the legacy DefaultGODEBUG httpmuxgo121=1 into the binary,
            ;; which disables Go 1.22 method patterns ("GET /v0/cities") on
            ;; net/http.ServeMux — the supervisor then 404s its entire typed
            ;; API while still serving the SPA.  A //go:debug directive in
            ;; package main restores the modern mux semantics.
            (lambda _
              (substitute* "src/github.com/gastownhall/gascity/cmd/gc/main.go"
                (("^package main")
                 "//go:debug httpmuxgo121=0\npackage main"))))
          (add-before 'build 'set-home
            (lambda _
              (setenv "HOME" "/tmp")))
          (replace 'build
            (lambda* (#:key build-flags import-path unpack-path
                      #:allow-other-keys)
              (let ((module-dir (string-append "src/" unpack-path)))
                (with-directory-excursion module-dir
                  (apply invoke "go" "install" "-trimpath"
                         `(,@build-flags ,import-path))))))
          (add-after 'install 'install-completions
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (gc (string-append out "/bin/gc"))
                     (bash-dir (string-append out "/etc/bash_completion.d"))
                     (zsh-dir (string-append out "/share/zsh/site-functions"))
                     (fish-dir (string-append out
                                "/share/fish/vendor_completions.d")))
                (mkdir-p bash-dir)
                (mkdir-p zsh-dir)
                (mkdir-p fish-dir)
                (with-output-to-file (string-append bash-dir "/gc")
                  (lambda ()
                    (system* gc "completion" "bash")))
                (with-output-to-file (string-append zsh-dir "/_gc")
                  (lambda ()
                    (system* gc "completion" "zsh")))
                (with-output-to-file (string-append fish-dir "/gc.fish")
                  (lambda ()
                    (system* gc "completion" "fish"))))))
          (add-after 'install 'install-examples
            ;; Ship the whole upstream `examples/' tree (~1.9 MiB,
            ;; text/config only) so a Guix home service can bootstrap
            ;; any example city with `gc init --from
            ;; <out>/share/gascity/examples/<name>' — gastown,
            ;; t3bridge-gastown, swarm, lifecycle, hyperscale, bd.
            ;; Each example
            ;; city carries its own self-contained packs/ subtree.
            (lambda* (#:key outputs unpack-path #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (src (string-append "src/" unpack-path "/examples"))
                     (dst (string-append out "/share/gascity/examples")))
                (mkdir-p (dirname dst))
                (copy-recursively src dst)))))))
    (native-inputs (list icu4c
                    go-github-com-burntsushi-toml
                    go-github-com-cespare-xxhash-v2
                    go-github-com-danielgtaylor-huma-v2
                    go-github-com-dolthub-dolt-go-v2
                    go-github-com-dolthub-driver-v2
                    ;; Transitive dolt CLI dependencies needed for compilation
                    ;; of the full dolt source tree (pulled in via the beads
                    ;; native Dolt store).
                    go-github-com-abiosoft-readline
                    go-github-com-andreyvit-diff
                    go-github-com-dolthub-ishell
                    go-github-com-flynn-archive-go-shlex
                    go-github-com-google-go-github-v57
                    go-github-com-google-shlex
                    go-github-com-pkg-profile
                    go-github-com-skratchdot-open-golang
                    go-github-com-tealeg-xlsx
                    go-github-com-fsnotify-fsnotify
                    go-github-com-gastownhall-gascity-packs
                    go-github-com-go-jose-go-jose-v4
                    go-github-com-go-logr-stdr
                    go-github-com-go-sql-driver-mysql
                    go-github-com-golang-jwt-jwt-v5
                    go-github-com-google-uuid
                    go-github-com-gorilla-websocket
                    go-github-com-invopop-jsonschema
                    go-github-com-masterminds-semver-v3
                    go-github-com-oapi-codegen-runtime
                    go-github-com-rogpeppe-go-internal
                    go-github-com-shirou-gopsutil-v4/fixed
                    go-github-com-spf13-cobra
                    go-github-com-spf13-pflag
                    go-github-com-steveyegge-beads-next
                    go-github-com-stretchr-testify
                    go-go-opentelemetry-io-auto-sdk
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
                    go-golang-org-x-sync
                    go-golang-org-x-sys
                    go-golang-org-x-term
                    go-golang-org-x-text
                    go-golang-org-x-time
                    go-google-golang-org-grpc
                    go-gopkg-in-yaml-v3
                    go-k8s-io-api
                    go-k8s-io-apimachinery
                    go-k8s-io-client-go
                    go-pgregory-net-rapid))
    ;; The gate PATH of a Gas City convergence check is rebuilt from the
    ;; directories of bd/gc/dolt/jq plus /usr/local/bin:/usr/bin:/bin (empty
    ;; on Guix System), so every tool a bundled check script looks up on PATH
    ;; must live in the same profile as gc itself: coreutils and grep cover
    ;; tr/dirname/mktemp/grep, and python-for-gascity is a python3 whose
    ;; baked-in GUIX_PYTHONPATH makes `import yaml` work under the gate's
    ;; env whitelist.
    (propagated-inputs (list beads-next
                             coreutils
                             dolt
                             grep
                             jq
                             lsof
                             procps
                             python-for-gascity
                             tmux
                             util-linux))
    (home-page "https://github.com/gastownhall/gascity")
    (synopsis "Orchestration SDK for multi-agent workflows")
    (description
     "@command{gc} (Gas City) is an orchestration-builder SDK for multi-agent
coding workflows.  It extracts the reusable infrastructure from Gastown into
a configurable toolkit with runtime providers, work routing, formulas, orders,
health patrol, and a declarative city configuration.  It supports multiple
runtime backends including tmux, subprocess, exec, ACP, and Kubernetes.")
    (license license:expat)))
