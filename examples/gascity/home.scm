;;; A minimal, runnable Home Gas City configuration.
;;;
;;; It runs one user-level Gas City supervisor with a single trivial city
;;; named "swarm" that uses the builtin "claude" provider.  Everything uses
;;; the Home service defaults: the state and log directories live under
;;; $XDG_STATE_HOME/gascity (or $HOME/.local/state/gascity), GC_HOME is
;;; $HOME/.gc, and the supervisor runs as the user that owns the Home
;;; environment.  The host's $HOME is never hardcoded here.
;;;
;;; Build the environment without installing anything:
;;;
;;;   guix home build -L modules examples/gascity/home.scm
;;;
;;; Run its activation and a command inside a container (the container gets a
;;; volatile /tmp and a tmpfs over $HOME, so nothing under the host home is
;;; touched).  Add the channel's modules to the load path from the repository
;;; root with -L modules:
;;;
;;;   guix home container -L modules examples/gascity/home.scm -- gc --version
;;;
;;; scripts/gascity-container-smoke-test home wraps that invocation and
;;; asserts the materialized city.toml, pack.toml and supervisor.toml.

(use-modules (gnu home)
             (gnu home services)
             (gnu services)
             (r0man guix home services gascity)
             (r0man guix toml))

(home-environment
 (services
  (append
   (list
    (service home-gascity-service-type
             (gascity-service-config
              ;; Dolt requires a commit author identity for managed bd
              ;; storage; without one `gc init' fails at genesis.
              (dolt-user-name "Gas City")
              (dolt-user-email "gascity@example.com")
              (cities
               (list
                (gascity-city-configuration
                 (name "swarm")
                 ;; Fetch nothing: the example is self-contained and offline.
                 (install-packs? #f)
                 (workspace (gascity-workspace-configuration
                             (name "swarm")
                             (provider "claude")))
                 (providers
                  (list (cons "claude"
                              (gascity-provider-configuration
                               (base "builtin:claude")))))
                 (daemon (gascity-daemon-configuration
                          (patrol-interval "30s")
                          (max-restarts 5)))
                 (pack (gascity-pack-configuration
                        (name "swarm")
                        (schema 2)
                        ;; The builtin `core' and `bd' packs provide the
                        ;; bead script `gc init' runs at genesis.  The gc
                        ;; binary pre-seeds these pinned versions offline, so
                        ;; `install-packs? #f' still fetches nothing.
                        (imports
                         (list
                          (cons "core"
                                (gascity-import-configuration
                                 (source "https://github.com/gastownhall/gascity.git//internal/bootstrap/packs/core")
                                 (version "sha:f895c0ff47d6ee9334ed282a416387eb5b084d24")))
                          (cons "bd"
                                (gascity-import-configuration
                                 (source "https://github.com/gastownhall/gascity.git//examples/bd")
                                 (version "sha:f895c0ff47d6ee9334ed282a416387eb5b084d24")))))))
                 ;; The extra-config escape hatch adds tables that have no
                 ;; typed record yet; both are known to the city schema.
                 (extra-config
                  (list (toml-table 'mail
                                    (toml-field 'provider "smtp")
                                    (toml-field 'retention_ttl "24h"))
                        (toml-table 'api
                                    (toml-field 'bind "127.0.0.1"))))))))))
   %base-home-services)))
