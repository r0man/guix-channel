(define-module (r0man guix packages task-management)
    #:use-module ((guix licenses) #:prefix license:)
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
    #:use-module (gnu packages icu4c)
    #:use-module (gnu packages linux)
    #:use-module (gnu packages lsof)
    #:use-module (gnu packages tmux)
    #:use-module (gnu packages web)
    #:use-module (gnu packages version-control)
    #:use-module (gnu packages)
    #:use-module (guix build-system go)
    #:use-module (guix gexp)
    #:use-module (guix git-download)
    #:use-module (guix packages)
    #:use-module (r0man guix packages golang-charm)
    #:use-module (r0man guix packages golang-dolthub)
    #:use-module (r0man guix packages golang-web)
    #:use-module (r0man guix packages golang-xyz))

(define-public beads-next
  (let ((commit "ac15e099e33de515406f6ddb851c202cdedaf607")
        (revision "32"))
    (package
      (name "beads-next")
      (version (git-version "1.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gastownhall/beads")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ii9zkiim1by9kjwb64lvrf8wc49d59dhxvk392j5gfh427wvykv"))))
      (build-system go-build-system)
      (arguments
       (list
        #:go go-1.25
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
      (license license:expat))))

(define-public go-github-com-steveyegge-beads
  (let ((commit "d57dc7128a8657bcd462a4aa3a66da92d227a15a")
        (revision "25"))
    (package
      (name "go-github-com-steveyegge-beads")
      (version (git-version "1.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gastownhall/beads")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1jj9m5wis2hpa7by1is07jvywja4w9jgkg30vzik5iwrmsp23v2b"))))
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

(define-public gastown-next
  (let ((commit "b1dc37c75dbd0404e81e024385d77115b908eafe")
        (revision "6995"))
    (package
      (name "gastown-next")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gastownhall/gastown")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1yaqh5y36pwmss2v981x1byvw6ycp8ph7y14h4imr2f89fspdx04"))))
      (build-system go-build-system)
      (arguments
       (list
        #:go go-1.25
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
      (license license:expat))))

(define-public gascity-next
  (let ((commit "45e4ca1d4b6e3e9fe225f1c94a09a175cc584575")
        (revision "1347"))
    (package
      (name "gascity-next")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gastownhall/gascity")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "15f0mc4zihqppm4kwpas1i40yihq9wf90dvnzcbnwfb58miqwq1h"))))
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
        #:phases
        #~(modify-phases %standard-phases
            (delete 'check)
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
                      (system* gc "completion" "fish")))))))))
      (native-inputs (list go-github-com-burntsushi-toml
                      go-github-com-cespare-xxhash-v2
                      go-github-com-danielgtaylor-huma-v2
                      go-github-com-fsnotify-fsnotify
                      go-github-com-go-logr-stdr
                      go-github-com-go-sql-driver-mysql
                      go-github-com-invopop-jsonschema
                      go-github-com-oapi-codegen-runtime
                      go-github-com-rogpeppe-go-internal
                      go-github-com-spf13-cobra
                      go-github-com-spf13-pflag
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
      (propagated-inputs (list beads-next
                               dolt
                               jq
                               lsof
                               procps
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
      (license license:expat))))
