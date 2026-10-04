;;; An interactive, ephemeral Home Gas City demo: a fresh "bright-lights" city.
;;;
;;; It runs one user-level Gas City supervisor with a single city named
;;; "bright-lights" that uses the builtin "claude" provider, the same minimal
;;; shape as examples/gascity/home.scm.  The city also declares the tutorial's
;;; project rig `~/my-project', so `cd ~/my-project && gc sling
;;; my-project/claude ...' has a target out of the box (the wrapper runs the
;;; idempotent `gc rig add ~/my-project' before dropping into the shell).  On
;;; top of that it puts the Claude
;;; Code CLI on both the supervisor's agent PATH (so `gc sling' can spawn a
;;; claude agent) and the interactive user's PATH (so `claude' can be run by
;;; hand), and it shares the host's Claude Code state so an existing login
;;; keeps working inside the container.
;;;
;;; The state and log directories live under $XDG_STATE_HOME/gascity (or
;;; $HOME/.local/state/gascity) and GC_HOME is $HOME/.gc, so `guix home
;;; container' keeps the whole city on its volatile tmpfs: nothing is written
;;; to the host home except what Claude Code itself writes under the shared
;;; ~/.claude.
;;;
;;; scripts/gascity-home-container wraps the invocation, waits for the
;;; supervisor, shares the host Claude Code state and drops the user into a
;;; login shell.  Build the environment without installing anything:
;;;
;;;   guix home build -L modules examples/gascity/bright-lights-home.scm

(use-modules (gnu home)
             (gnu home services)
             (gnu packages base)
             (gnu packages gawk)
             (gnu packages version-control)
             (gnu services)
             (guix gexp)
             (r0man guix home services gascity)
             (r0man guix packages claude)
             (r0man guix toml))

(home-environment
 ;; On the interactive user's PATH: claude-code, plus the git/sed/gawk that
 ;; `gc rig add' and the bundled bead shim (`gc-beads-bd.sh') look up by
 ;; name (the supervisor feeds them to its agents separately).
 (packages
  (list claude-code git sed gawk))
 (services
  (append
   (list
    (service home-gascity-service-type
             (gascity-service-config
              ;; Dolt requires a commit author identity for managed bd
              ;; storage; without one `gc init' fails at genesis.
              (dolt-user-name "Gas City")
              (dolt-user-email "gascity@example.com")
              ;; claude-code on the supervisor's agent PATH so spawned claude
              ;; sessions resolve the binary by name.
              (packages (list claude-code))
              ;; The container shares the host network namespace, so the
              ;; supervisor must not sit on the default 8372 (a host
              ;; supervisor likely owns it).  GASCITY_DEMO_PORT overrides the
              ;; default for the rare host that already uses 18372.
              (port (or (and=> (getenv "GASCITY_DEMO_PORT") string->number)
                        18372))
              (cities
               (list
                (gascity-city-configuration
                 (name "bright-lights")
                 ;; Fetch nothing: the example is self-contained and offline.
                 (install-packs? #f)
                 (workspace (gascity-workspace-configuration
                             (name "bright-lights")
                             (provider "claude")))
                 (providers
                  (list (cons "claude"
                              (gascity-provider-configuration
                               (base "builtin:claude")))))
                 ;; The tutorial's project rig, declared so it has a name,
                 ;; prefix and path.  HOME is the container's tmpfs home, so
                 ;; the rig lives and dies with the demo.
                 (rigs
                  (list
                   (gascity-rig-configuration
                    (name "my-project")
                    (path (string-append (getenv "HOME") "/my-project"))
                    (prefix "mp"))))
                 (daemon (gascity-daemon-configuration
                          (patrol-interval "30s")
                          (max-restarts 5)))
                 (pack (gascity-pack-configuration
                        (name "bright-lights")
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
                                    (toml-field 'bind "127.0.0.1")))))))))
    ;; The supervisor runs with HOME=$XDG_STATE_HOME/gascity, so claude would
    ;; look for its config there and miss the shared host ~/.claude and
    ;; ~/.claude.json: sessions then hang at the first-run theme prompt and
    ;; the session start times out.  Link the shared files into the state home
    ;; before the supervisor starts; the links are ephemeral, like the rest of
    ;; the demo.
    (simple-service 'gascity-share-claude-config
                    home-activation-service-type
                    #~(begin
                        (use-modules (guix build utils))
                        (let* ((home (getenv "HOME"))
                               (state (string-append
                                       (or (getenv "XDG_STATE_HOME")
                                           (string-append home "/.local/state"))
                                       "/gascity")))
                          (mkdir-p state)
                          (for-each
                           (lambda (entry)
                             (let ((source (car entry))
                                   (target (cdr entry)))
                               (when (file-exists? source)
                                 (false-if-exception (delete-file target))
                                 (symlink source target))))
                           (list (cons (string-append home "/.claude")
                                       (string-append state "/.claude"))
                                 (cons (string-append home "/.claude.json")
                                       (string-append state "/.claude.json"))))))))
   %base-home-services)))
