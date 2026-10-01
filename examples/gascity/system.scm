;;; A minimal, runnable system Gas City configuration.
;;;
;;; It runs one machine-wide Gas City supervisor with a single trivial city
;;; named "swarm" that uses the builtin "claude" provider, the same shape as
;;; examples/gascity/home.scm.  Everything uses the service defaults: the
;;; supervisor runs as the "gascity" account, its state directory is
;;; /var/lib/gascity, its log directory /var/log/gascity, and its GC_HOME
;;; /var/lib/gascity/.gc.
;;;
;;; The operating system inherits `simple-operating-system' (from (gnu tests),
;;; the small OS used throughout Guix's own tests) so the file is directly
;;; runnable.  Build a container or a VM:
;;;
;;;   guix system build   -L modules examples/gascity/system.scm
;;;   guix system container -L modules examples/gascity/system.scm
;;;   guix system vm      -L modules examples/gascity/system.scm
;;;
;;; The system container smoke test is added separately (see
;;; docs/gascity-service.org, "Running the examples").

(use-modules (gnu)
             (gnu services)
             (gnu services base)
             (gnu tests)
             (r0man guix services gascity)
             (r0man guix toml))

(operating-system
 (inherit
  (simple-operating-system
   ;; The provision one-shot requires `networking'; `guix system vm' and
   ;; `guix system container' provide the QEMU user-mode network.
   (service static-networking-service-type
            (list %qemu-static-networking))
   (service gascity-service-type
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
                       ;; The builtin `core' and `bd' packs provide the bead
                       ;; script `gc init' runs at genesis.  The gc binary
                       ;; pre-seeds these pinned versions offline, so
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
                                   (toml-field 'bind "127.0.0.1"))))))))))))
