;;; Home service counterpart of (r0man guix services gascity).
;;;
;;; The service type is derived from the system service type, following the
;;; convention of upstream services such as (gnu home services syncthing) and
;;; (gnu home services mcron).  Like the system service, its value is a list
;;; of supervisor configurations, one per user-level supervisor process.
;;;
;;; As github-actions documents (@16.4), 'system->home-service-type' alone is
;;; not enough: the derived type keeps the system 'account-service-type'
;;; extension (a Home environment has no system accounts) and its system
;;; Shepherd and activation extensions hardcode a dedicated user and /var/...
;;; paths.  This module redefines the extensions explicitly.

(define-module (r0man guix home services gascity)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (r0man guix services gascity)
  #:use-module (srfi srfi-1)
  #:export (home-gascity-service-type
            home-gascity-state-directory
            home-gascity-log-directory
            home-gascity-gc-home
            home-gascity-supervisor-configuration
            home-gascity-supervisor-configurations
            home-gascity-supervisor-provision
            home-gascity-supervisor-shepherd-service-name)
  #:re-export (
            gascity-tri-state?
            gascity-maybe-string?
            gascity-maybe-string-or-gexp?
            gascity-serialize-string
            gascity-serialize-maybe-string
            gascity-serialize-string-or-gexp
            gascity-serialize-boolean
            gascity-serialize-maybe-boolean
            gascity-serialize-tri-state
            gascity-serialize-integer
            gascity-serialize-maybe-integer
            gascity-serialize-real
            gascity-serialize-maybe-real
            gascity-serialize-list-of-strings
            gascity-serialize-string-map
            gascity-serialize-subtable
            gascity-serialize-subtables
            gascity-serialize-alist-subtables
            gascity-empty-serializer
            gascity-merge-extra-config
            gascity-secret-value?
            gascity-sanitize-and-validate
            gascity-supervisor-configuration
            gascity-supervisor-configuration?
            gascity-supervisor-configuration-id
            gascity-supervisor-configuration-package
            gascity-supervisor-configuration-user
            gascity-supervisor-configuration-group
            gascity-supervisor-configuration-gc-home
            gascity-supervisor-configuration-state-directory
            gascity-supervisor-configuration-log-directory
            gascity-supervisor-configuration-bind
            gascity-supervisor-configuration-port
            gascity-supervisor-configuration-settings
            gascity-supervisor-configuration-packages
            gascity-supervisor-configuration-environment-variables
            gascity-supervisor-configuration-secrets-file
            gascity-supervisor-configuration-cities
            gascity-supervisor-settings-configuration
            gascity-supervisor-settings-configuration?
            gascity-supervisor-settings-configuration-port
            gascity-supervisor-settings-configuration-bind
            gascity-supervisor-settings-configuration-patrol-interval
            gascity-supervisor-settings-configuration-allow-mutations?
            gascity-supervisor-settings-configuration-allowed-origins
            gascity-supervisor-settings-configuration-allowed-hosts
            gascity-supervisor-settings-configuration-write-auth-verify-key
            gascity-supervisor-settings-configuration-write-auth-required?
            gascity-supervisor-settings-configuration-write-auth-allow-unverified?
            gascity-supervisor-settings-configuration-read-auth-verify-key
            gascity-supervisor-settings-configuration-read-auth-required?
            gascity-supervisor-settings-configuration-publication-provider
            gascity-supervisor-settings-configuration-publication-tenant-slug
            gascity-supervisor-settings-configuration-publication-public-base-domain
            gascity-supervisor-settings-configuration-publication-tenant-base-domain
            gascity-supervisor-settings-configuration-publication-tenant-auth-policy-ref
            gascity-supervisor-settings-configuration-events-export-endpoint
            gascity-supervisor-settings-configuration-events-export-cities
            gascity-supervisor-settings-configuration-events-export-token
            gascity-supervisor-settings-configuration-events-export-token-file
            gascity-supervisor-settings-configuration-events-export-actor-salt
            gascity-supervisor-settings-configuration-events-export-batch-max-events
            gascity-supervisor-settings-configuration-events-export-batch-interval
            gascity-supervisor-settings-configuration-events-export-export-ref
            gascity-city-configuration
            gascity-city-configuration?
            gascity-city-configuration-name
            gascity-city-configuration-directory
            gascity-city-configuration-package
            gascity-city-configuration-user
            gascity-city-configuration-enabled?
            gascity-city-configuration-genesis?
            gascity-city-configuration-install-packs?
            gascity-city-configuration-register?
            gascity-city-configuration-pack
            gascity-city-configuration-workspace
            gascity-city-configuration-providers
            gascity-city-configuration-upstreams
            gascity-city-configuration-imports
            gascity-city-configuration-agents
            gascity-city-configuration-named-sessions
            gascity-city-configuration-rigs
            gascity-city-configuration-patches
            gascity-city-configuration-daemon
            gascity-city-configuration-dolt
            gascity-city-configuration-storage
            gascity-city-configuration-beads
            gascity-city-configuration-session
            gascity-city-configuration-session-sleep
            gascity-city-configuration-defaults
            gascity-city-configuration-agent-defaults
            gascity-city-configuration-pricing
            gascity-city-configuration-files
            gascity-city-configuration-rig-files
            gascity-city-configuration-extra-config
            gascity-city-configuration-extra-toml
            gascity-city-configuration-include
            gascity-pack-configuration
            gascity-pack-configuration?
            gascity-pack-configuration-name
            gascity-pack-configuration-schema
            gascity-pack-configuration-version
            gascity-pack-configuration-requires-gc
            gascity-pack-configuration-description
            gascity-pack-configuration-includes
            gascity-pack-configuration-requires
            gascity-pack-configuration-imports
            gascity-pack-configuration-agent-defaults
            gascity-pack-configuration-agents
            gascity-pack-configuration-named-sessions
            gascity-pack-configuration-providers
            gascity-pack-configuration-upstreams
            gascity-pack-configuration-runtimes
            gascity-pack-configuration-patches
            gascity-pack-configuration-doctor
            gascity-pack-configuration-commands
            gascity-pack-configuration-global
            gascity-pack-configuration-pricing
            gascity-workspace-configuration
            gascity-workspace-configuration?
            gascity-workspace-configuration-name
            gascity-workspace-configuration-prefix
            gascity-workspace-configuration-provider
            gascity-workspace-configuration-timezone
            gascity-workspace-configuration-start-command
            gascity-workspace-configuration-suspended?
            gascity-workspace-configuration-suspended-on-start?
            gascity-workspace-configuration-max-active-sessions
            gascity-workspace-configuration-session-template
            gascity-workspace-configuration-install-agent-hooks
            gascity-workspace-configuration-global-fragments
            gascity-workspace-configuration-includes
            gascity-workspace-configuration-default-rig-includes
            gascity-workspace-configuration-env
            gascity-provider-configuration
            gascity-provider-configuration?
            gascity-provider-configuration-base
            gascity-provider-configuration-display-name
            gascity-provider-configuration-command
            gascity-provider-configuration-args
            gascity-provider-configuration-args-append
            gascity-provider-configuration-options-schema-merge
            gascity-provider-configuration-prompt-mode
            gascity-provider-configuration-prompt-flag
            gascity-provider-configuration-ready-delay-ms
            gascity-provider-configuration-ready-prompt-prefix
            gascity-provider-configuration-process-names
            gascity-provider-configuration-emits-permission-warning
            gascity-provider-configuration-accept-startup-dialogs?
            gascity-provider-configuration-env
            gascity-provider-configuration-path-check
            gascity-provider-configuration-supports-acp?
            gascity-provider-configuration-supports-hooks?
            gascity-provider-configuration-instructions-file
            gascity-provider-configuration-resume-flag
            gascity-provider-configuration-resume-style
            gascity-provider-configuration-resume-command
            gascity-provider-configuration-session-id-flag
            gascity-provider-configuration-fork-flag
            gascity-provider-configuration-option-defaults
            gascity-provider-configuration-print-args
            gascity-provider-configuration-title-model
            gascity-provider-configuration-acp-command
            gascity-provider-configuration-acp-args
            gascity-provider-patch-configuration
            gascity-provider-patch-configuration?
            gascity-provider-patch-configuration-name
            gascity-provider-patch-configuration-base
            gascity-provider-patch-configuration-command
            gascity-provider-patch-configuration-acp-command
            gascity-provider-patch-configuration-args
            gascity-provider-patch-configuration-acp-args
            gascity-provider-patch-configuration-args-append
            gascity-provider-patch-configuration-options-schema-merge
            gascity-provider-patch-configuration-prompt-mode
            gascity-provider-patch-configuration-prompt-flag
            gascity-provider-patch-configuration-ready-delay-ms
            gascity-provider-patch-configuration-accept-startup-dialogs?
            gascity-provider-patch-configuration-env
            gascity-provider-patch-configuration-env-remove
            gascity-provider-patch-configuration-replace?
            gascity-import-configuration
            gascity-import-configuration?
            gascity-import-configuration-source
            gascity-import-configuration-version
            gascity-agent-configuration
            gascity-agent-configuration?
            gascity-agent-configuration-name
            gascity-agent-configuration-description
            gascity-agent-configuration-dir
            gascity-agent-configuration-work-dir
            gascity-agent-configuration-tmux-alias
            gascity-agent-configuration-scope
            gascity-agent-configuration-suspended?
            gascity-agent-configuration-pre-start
            gascity-agent-configuration-prompt-template
            gascity-agent-configuration-nudge
            gascity-agent-configuration-session
            gascity-agent-configuration-provider
            gascity-agent-configuration-upstream
            gascity-agent-configuration-start-command
            gascity-agent-configuration-lifecycle
            gascity-agent-configuration-args
            gascity-agent-configuration-prompt-mode
            gascity-agent-configuration-prompt-flag
            gascity-agent-configuration-ready-delay-ms
            gascity-agent-configuration-ready-prompt-prefix
            gascity-agent-configuration-process-names
            gascity-agent-configuration-emits-permission-warning
            gascity-agent-configuration-env
            gascity-agent-configuration-option-defaults
            gascity-agent-configuration-max-active-sessions
            gascity-agent-configuration-min-active-sessions
            gascity-agent-configuration-scale-check
            gascity-agent-configuration-drain-timeout
            gascity-agent-configuration-on-boot
            gascity-agent-configuration-on-death
            gascity-agent-configuration-namepool
            gascity-agent-configuration-work-query
            gascity-agent-configuration-sling-query
            gascity-agent-configuration-idle-timeout
            gascity-agent-configuration-max-session-age
            gascity-agent-configuration-max-session-age-jitter
            gascity-agent-configuration-assigned-work-defer-limit
            gascity-agent-configuration-sleep-after-idle
            gascity-agent-configuration-auto-reclaim-stale-claims?
            gascity-agent-configuration-install-agent-hooks
            gascity-agent-configuration-skills
            gascity-agent-configuration-mcp
            gascity-agent-configuration-hooks-installed?
            gascity-agent-configuration-session-setup
            gascity-agent-configuration-session-setup-script
            gascity-agent-configuration-session-live
            gascity-agent-configuration-overlay-dir
            gascity-agent-configuration-default-sling-formula
            gascity-agent-configuration-inject-fragments
            gascity-agent-configuration-append-fragments
            gascity-agent-configuration-inject-assigned-skills
            gascity-agent-configuration-attach?
            gascity-agent-configuration-depends-on
            gascity-agent-configuration-resume-command
            gascity-agent-configuration-wake-mode
            gascity-agent-configuration-mouse-mode
            gascity-agent-defaults-configuration
            gascity-agent-defaults-configuration?
            gascity-agent-defaults-configuration-provider
            gascity-agent-defaults-configuration-model
            gascity-agent-defaults-configuration-upstream
            gascity-agent-defaults-configuration-wake-mode
            gascity-agent-defaults-configuration-default-sling-formula
            gascity-agent-defaults-configuration-allow-overlay
            gascity-agent-defaults-configuration-allow-env-override
            gascity-agent-defaults-configuration-append-fragments
            gascity-agent-defaults-configuration-skills
            gascity-agent-defaults-configuration-mcp
            gascity-agent-override-configuration
            gascity-agent-override-configuration?
            gascity-agent-override-configuration-agent
            gascity-agent-override-configuration-dir
            gascity-agent-override-configuration-work-dir
            gascity-agent-override-configuration-tmux-alias
            gascity-agent-override-configuration-scope
            gascity-agent-override-configuration-suspended?
            gascity-agent-override-configuration-env
            gascity-agent-override-configuration-env-remove
            gascity-agent-override-configuration-pre-start
            gascity-agent-override-configuration-prompt-template
            gascity-agent-override-configuration-session
            gascity-agent-override-configuration-provider
            gascity-agent-override-configuration-upstream
            gascity-agent-override-configuration-args
            gascity-agent-override-configuration-start-command
            gascity-agent-override-configuration-lifecycle
            gascity-agent-override-configuration-nudge
            gascity-agent-override-configuration-idle-timeout
            gascity-agent-override-configuration-max-session-age
            gascity-agent-override-configuration-max-session-age-jitter
            gascity-agent-override-configuration-assigned-work-defer-limit
            gascity-agent-override-configuration-sleep-after-idle
            gascity-agent-override-configuration-auto-reclaim-stale-claims?
            gascity-agent-override-configuration-install-agent-hooks
            gascity-agent-override-configuration-skills
            gascity-agent-override-configuration-mcp
            gascity-agent-override-configuration-hooks-installed?
            gascity-agent-override-configuration-inject-assigned-skills
            gascity-agent-override-configuration-session-setup
            gascity-agent-override-configuration-session-setup-script
            gascity-agent-override-configuration-session-live
            gascity-agent-override-configuration-overlay-dir
            gascity-agent-override-configuration-default-sling-formula
            gascity-agent-override-configuration-inject-fragments
            gascity-agent-override-configuration-append-fragments
            gascity-agent-override-configuration-pre-start-append
            gascity-agent-override-configuration-session-setup-append
            gascity-agent-override-configuration-session-live-append
            gascity-agent-override-configuration-install-agent-hooks-append
            gascity-agent-override-configuration-skills-append
            gascity-agent-override-configuration-mcp-append
            gascity-agent-override-configuration-inject-fragments-append
            gascity-agent-override-configuration-attach?
            gascity-agent-override-configuration-depends-on
            gascity-agent-override-configuration-resume-command
            gascity-agent-override-configuration-wake-mode
            gascity-agent-override-configuration-mouse-mode
            gascity-agent-override-configuration-max-active-sessions
            gascity-agent-override-configuration-min-active-sessions
            gascity-agent-override-configuration-scale-check
            gascity-agent-override-configuration-option-defaults
            gascity-agent-patch-configuration
            gascity-agent-patch-configuration?
            gascity-agent-patch-configuration-name
            gascity-agent-patch-configuration-dir
            gascity-agent-patch-configuration-rig
            gascity-agent-patch-configuration-work-dir
            gascity-agent-patch-configuration-tmux-alias
            gascity-agent-patch-configuration-scope
            gascity-agent-patch-configuration-suspended?
            gascity-agent-patch-configuration-env
            gascity-agent-patch-configuration-env-remove
            gascity-agent-patch-configuration-pre-start
            gascity-agent-patch-configuration-prompt-template
            gascity-agent-patch-configuration-session
            gascity-agent-patch-configuration-provider
            gascity-agent-patch-configuration-upstream
            gascity-agent-patch-configuration-args
            gascity-agent-patch-configuration-start-command
            gascity-agent-patch-configuration-lifecycle
            gascity-agent-patch-configuration-nudge
            gascity-agent-patch-configuration-idle-timeout
            gascity-agent-patch-configuration-max-session-age
            gascity-agent-patch-configuration-max-session-age-jitter
            gascity-agent-patch-configuration-assigned-work-defer-limit
            gascity-agent-patch-configuration-sleep-after-idle
            gascity-agent-patch-configuration-auto-reclaim-stale-claims?
            gascity-agent-patch-configuration-install-agent-hooks
            gascity-agent-patch-configuration-skills
            gascity-agent-patch-configuration-mcp
            gascity-agent-patch-configuration-skills-append
            gascity-agent-patch-configuration-mcp-append
            gascity-agent-patch-configuration-hooks-installed?
            gascity-agent-patch-configuration-inject-assigned-skills
            gascity-agent-patch-configuration-session-setup
            gascity-agent-patch-configuration-session-setup-script
            gascity-agent-patch-configuration-session-live
            gascity-agent-patch-configuration-overlay-dir
            gascity-agent-patch-configuration-default-sling-formula
            gascity-agent-patch-configuration-inject-fragments
            gascity-agent-patch-configuration-append-fragments
            gascity-agent-patch-configuration-attach?
            gascity-agent-patch-configuration-depends-on
            gascity-agent-patch-configuration-resume-command
            gascity-agent-patch-configuration-wake-mode
            gascity-agent-patch-configuration-mouse-mode
            gascity-agent-patch-configuration-pre-start-append
            gascity-agent-patch-configuration-session-setup-append
            gascity-agent-patch-configuration-session-live-append
            gascity-agent-patch-configuration-install-agent-hooks-append
            gascity-agent-patch-configuration-inject-fragments-append
            gascity-agent-patch-configuration-max-active-sessions
            gascity-agent-patch-configuration-min-active-sessions
            gascity-agent-patch-configuration-scale-check
            gascity-agent-patch-configuration-option-defaults
            gascity-named-session-configuration
            gascity-named-session-configuration?
            gascity-named-session-configuration-name
            gascity-named-session-configuration-template
            gascity-named-session-configuration-scope
            gascity-named-session-configuration-dir
            gascity-named-session-configuration-mode
            gascity-rig-configuration
            gascity-rig-configuration?
            gascity-rig-configuration-name
            gascity-rig-configuration-path
            gascity-rig-configuration-prefix
            gascity-rig-configuration-default-branch
            gascity-rig-configuration-suspended?
            gascity-rig-configuration-suspended-on-start?
            gascity-rig-configuration-formulas-dir
            gascity-rig-configuration-includes
            gascity-rig-configuration-imports
            gascity-rig-configuration-max-active-sessions
            gascity-rig-configuration-overrides
            gascity-rig-configuration-patches
            gascity-rig-configuration-default-sling-target
            gascity-rig-configuration-default-sling-targets
            gascity-rig-configuration-session-sleep
            gascity-rig-configuration-dolt-host
            gascity-rig-configuration-dolt-port
            gascity-rig-configuration-formula-vars
            gascity-rig-patch-configuration
            gascity-rig-patch-configuration?
            gascity-rig-patch-configuration-name
            gascity-rig-patch-configuration-path
            gascity-rig-patch-configuration-prefix
            gascity-rig-patch-configuration-default-branch
            gascity-rig-patch-configuration-suspended?
            gascity-rig-patch-configuration-suspended-on-start?
            gascity-rig-patch-configuration-formula-vars
            gascity-patches-configuration
            gascity-patches-configuration?
            gascity-patches-configuration-agents
            gascity-patches-configuration-rigs
            gascity-patches-configuration-providers
            gascity-pack-defaults-configuration
            gascity-pack-defaults-configuration?
            gascity-pack-defaults-configuration-rig
            gascity-pack-rig-defaults-configuration
            gascity-pack-rig-defaults-configuration?
            gascity-pack-rig-defaults-configuration-imports
            gascity-daemon-configuration
            gascity-daemon-configuration?
            gascity-daemon-configuration-formula-v2
            gascity-daemon-configuration-graph-workflows?
            gascity-daemon-configuration-patrol-interval
            gascity-daemon-configuration-max-restarts
            gascity-daemon-configuration-restart-window
            gascity-daemon-configuration-session-circuit-breaker?
            gascity-daemon-configuration-session-circuit-breaker-max-restarts
            gascity-daemon-configuration-session-circuit-breaker-window
            gascity-daemon-configuration-session-circuit-breaker-reset-after
            gascity-daemon-configuration-shutdown-timeout
            gascity-daemon-configuration-dolt-stop-timeout
            gascity-daemon-configuration-dolt-start-address-in-use-retry-window
            gascity-daemon-configuration-wisp-gc-interval
            gascity-daemon-configuration-wisp-ttl
            gascity-daemon-configuration-drift-drain-timeout
            gascity-daemon-configuration-observe-paths
            gascity-daemon-configuration-probe-concurrency
            gascity-daemon-configuration-max-wakes-per-tick
            gascity-daemon-configuration-nudge-dispatcher
            gascity-daemon-configuration-auto-restart-on-drift?
            gascity-daemon-configuration-auto-reap-closed-bead-worktrees?
            gascity-daemon-configuration-auto-reap-closed-bead-worktrees-dry-run?
            gascity-daemon-configuration-auto-reap-closed-bead-worktrees-min-age-minutes
            gascity-daemon-configuration-start-ready-timeout
            gascity-daemon-configuration-tick-debounce
            gascity-daemon-configuration-auto-prune-worker-dir?
            gascity-dolt-configuration
            gascity-dolt-configuration?
            gascity-dolt-configuration-port
            gascity-dolt-configuration-host
            gascity-dolt-configuration-archive-level
            gascity-dolt-configuration-auto-gc-enabled?
            gascity-dolt-configuration-max-connections
            gascity-dolt-configuration-read-timeout-millis
            gascity-dolt-configuration-write-timeout-millis
            gascity-dolt-configuration-wait-timeout-seconds
            gascity-dolt-configuration-dolt-lock-release-timeout
            gascity-storage-configuration
            gascity-storage-configuration?
            gascity-storage-configuration-classes
            gascity-storage-classes-configuration
            gascity-storage-classes-configuration?
            gascity-storage-classes-configuration-work
            gascity-storage-classes-configuration-graph
            gascity-storage-classes-configuration-sessions
            gascity-storage-classes-configuration-messaging
            gascity-storage-classes-configuration-orders
            gascity-storage-classes-configuration-nudges
            gascity-beads-configuration
            gascity-beads-configuration?
            gascity-beads-configuration-provider
            gascity-beads-configuration-backend
            gascity-beads-configuration-event-hooks?
            gascity-beads-configuration-bd-compatibility
            gascity-beads-configuration-conditional-writes
            gascity-beads-configuration-guarded-release
            gascity-session-configuration
            gascity-session-configuration?
            gascity-session-configuration-provider
            gascity-session-configuration-setup-timeout
            gascity-session-configuration-setup-max-timeout
            gascity-session-configuration-nudge-ready-timeout
            gascity-session-configuration-nudge-retry-interval
            gascity-session-configuration-nudge-poll-interval
            gascity-session-configuration-nudge-lock-timeout
            gascity-session-configuration-debounce-ms
            gascity-session-configuration-display-ms
            gascity-session-configuration-startup-timeout
            gascity-session-configuration-progress-stall-timeout
            gascity-session-configuration-claim-holder-stall-timeout
            gascity-session-configuration-socket
            gascity-session-configuration-remote-match
            gascity-session-sleep-configuration
            gascity-session-sleep-configuration?
            gascity-session-sleep-configuration-interactive-resume
            gascity-session-sleep-configuration-interactive-fresh
            gascity-session-sleep-configuration-noninteractive
            gascity-upstream-configuration
            gascity-upstream-configuration?
            gascity-upstream-configuration-description
            gascity-upstream-configuration-base-url
            gascity-upstream-configuration-api-key
            gascity-upstream-configuration-auth-token
            gascity-upstream-configuration-base-url-env
            gascity-upstream-configuration-api-key-env
            gascity-upstream-configuration-auth-token-env
            gascity-upstream-configuration-env
            gascity-model-pricing-configuration
            gascity-model-pricing-configuration?
            gascity-model-pricing-configuration-provider
            gascity-model-pricing-configuration-model
            gascity-model-pricing-configuration-tier
            gascity-model-pricing-configuration-last-verified
            gascity-pricing-tier-configuration
            gascity-pricing-tier-configuration?
            gascity-pricing-tier-configuration-prompt-usd-per-1m
            gascity-pricing-tier-configuration-completion-usd-per-1m
            gascity-pricing-tier-configuration-cache-read-usd-per-1m
            gascity-pricing-tier-configuration-cache-creation-usd-per-1m
            gascity-pack-requirement-configuration
            gascity-pack-requirement-configuration?
            gascity-pack-requirement-configuration-scope
            gascity-pack-requirement-configuration-agent
            gascity-pack-patches-configuration
            gascity-pack-patches-configuration?
            gascity-pack-patches-configuration-agents
            gascity-pack-global-configuration
            gascity-pack-global-configuration?
            gascity-pack-global-configuration-session-live
            gascity-pack-runtime-entry-configuration
            gascity-pack-runtime-entry-configuration?
            gascity-pack-runtime-entry-configuration-command
            gascity-pack-runtime-entry-configuration-protocol
            gascity-pack-runtime-entry-configuration-prompt-delivery
            gascity-pack-doctor-entry-configuration
            gascity-pack-doctor-entry-configuration?
            gascity-pack-doctor-entry-configuration-name
            gascity-pack-doctor-entry-configuration-script
            gascity-pack-doctor-entry-configuration-description
            gascity-pack-doctor-entry-configuration-fix
            gascity-pack-doctor-entry-configuration-warmup?
            gascity-pack-command-entry-configuration
            gascity-pack-command-entry-configuration?
            gascity-pack-command-entry-configuration-name
            gascity-pack-command-entry-configuration-description
            gascity-pack-command-entry-configuration-long-description
            gascity-pack-command-entry-configuration-script
            gascity-supervisor-configuration->toml
            gascity-supervisor-settings-configuration->toml
            gascity-city-configuration->toml
            gascity-pack-configuration->toml
            gascity-workspace-configuration->toml
            gascity-provider-configuration->toml
            gascity-provider-patch-configuration->toml
            gascity-import-configuration->toml
            gascity-agent-configuration->toml
            gascity-agent-defaults-configuration->toml
            gascity-agent-override-configuration->toml
            gascity-agent-patch-configuration->toml
            gascity-named-session-configuration->toml
            gascity-rig-configuration->toml
            gascity-rig-patch-configuration->toml
            gascity-patches-configuration->toml
            gascity-pack-defaults-configuration->toml
            gascity-pack-rig-defaults-configuration->toml
            gascity-daemon-configuration->toml
            gascity-dolt-configuration->toml
            gascity-storage-configuration->toml
            gascity-storage-classes-configuration->toml
            gascity-beads-configuration->toml
            gascity-session-configuration->toml
            gascity-session-sleep-configuration->toml
            gascity-upstream-configuration->toml
            gascity-model-pricing-configuration->toml
            gascity-pricing-tier-configuration->toml
            gascity-pack-requirement-configuration->toml
            gascity-pack-patches-configuration->toml
            gascity-pack-global-configuration->toml
            gascity-pack-runtime-entry-configuration->toml
            gascity-pack-doctor-entry-configuration->toml
            gascity-pack-command-entry-configuration->toml
            gascity-supervisor-settings->toml-document
            gascity-city->toml-document
            gascity-pack->toml-document
            gascity-supervisor-settings->toml-string
            gascity-city->toml-string
            gascity-pack->toml-string
            ;; System service type and lifecycle (Phase 3).
            gascity-service-config
            gascity-service-type
            gascity-supervisor-configurations
            gascity-supervisor-provision
            gascity-supervisor-shepherd-service-name
            gascity-supervisor-user
            gascity-supervisor-group
            gascity-supervisor-state-directory
            gascity-supervisor-log-directory
            gascity-supervisor-gc-home
            gascity-supervisor-port
            gascity-supervisor-secrets-file
            gascity-supervisor-log-file
            gascity-supervisor-environment
            gascity-city-directory
            gascity-city-site->toml-string
            gascity-cities-toml-merge
            gascity-supervisor-activation-program
            gascity-supervisor-provision-program
            gascity-supervisor-program))

;;;
;;; Home path defaults.
;;;
;;; The system service derives its paths from the account's home under
;;; /var/lib.  The Home service derives them from the user's environment
;;; instead, resolved when the Home environment is built: the state and log
;;; directories under $XDG_STATE_HOME (or $HOME/.local/state), and GC_HOME
;;; under $HOME, matching the interactive `gc' default.

(define (home-gascity-state-home)
  ;; $XDG_STATE_HOME, resolved when the Home environment is built.
  (or (getenv "XDG_STATE_HOME")
      (string-append (getenv "HOME") "/.local/state")))

(define (home-gascity-state-directory)
  "Return the state directory of the Home Gas City service:
$XDG_STATE_HOME/gascity, or $HOME/.local/state/gascity."
  (string-append (home-gascity-state-home) "/gascity"))

(define (home-gascity-log-directory)
  "Return the log directory of the Home Gas City service: the state
directory."
  (home-gascity-state-directory))

(define (home-gascity-gc-home)
  "Return the default GC_HOME of the Home Gas City service: $HOME/.gc, the
registry, settings, control socket and instance lock of a user supervisor."
  (string-append (getenv "HOME") "/.gc"))


;;;
;;; Configuration normalization.
;;;

(define (home-gascity-supervisor-configuration config)
  "Return the <gascity-supervisor-configuration> CONFIG with the Home path
defaults filled in: `state-directory' and `log-directory' default to
$XDG_STATE_HOME/gascity, and `gc-home' to $HOME/.gc.  The `user' and `group'
fields are ignored: the supervisor runs as the user that owns the Home
environment."
  (gascity-supervisor-configuration
   (inherit config)
   (state-directory
    (let ((directory (gascity-supervisor-configuration-state-directory config)))
      (if (eq? directory 'unset) (home-gascity-state-directory) directory)))
   (log-directory
    (let ((directory (gascity-supervisor-configuration-log-directory config)))
      (if (eq? directory 'unset) (home-gascity-log-directory) directory)))
   (gc-home
    (let ((directory (gascity-supervisor-configuration-gc-home config)))
      (if (eq? directory 'unset) (home-gascity-gc-home) directory)))))

(define (home-gascity-supervisor-configurations value)
  "Return the normalized, home-defaulted supervisor configurations of the
Home service VALUE, validating it as the system service does.  The paths of
each configuration default to the Home paths; an invalid VALUE is passed on
unchanged so that the system validation raises the same error."
  (gascity-supervisor-configurations
   (if (and (list? value)
            (every gascity-supervisor-configuration? value))
       (map home-gascity-supervisor-configuration value)
       value)))


;;;
;;; Shepherd service names.
;;;

(define (home-gascity-supervisor-provision config)
  "Return the Home Shepherd service name of the provision one-shot of the
supervisor instance CONFIG: `home-gascity-provision', suffixed with its id."
  (let ((id (gascity-supervisor-configuration-id config)))
    (if (and (string? id) (not (string-null? id)))
        (string->symbol (string-append "home-gascity-provision-" id))
        'home-gascity-provision)))

(define (home-gascity-supervisor-shepherd-service-name config)
  "Return the Home Shepherd service name of the supervisor instance CONFIG:
`home-gascity-supervisor', suffixed with its id."
  (let ((id (gascity-supervisor-configuration-id config)))
    (if (and (string? id) (not (string-null? id)))
        (string->symbol (string-append "home-gascity-supervisor-" id))
        'home-gascity-supervisor)))


;;;
;;; Shepherd services.
;;;

(define (home-gascity-supervisor-provision-shepherd-service config)
  "Return the one-shot Home Shepherd service of the supervisor instance
CONFIG: it runs the shared provision program as the current user and waits
for it.  There is no `#:user'/`#:group' and no `networking'/`user-processes'
requirement: those only exist in the system Shepherd."
  (shepherd-service
   (documentation
    "Provision a Gas City city as a user service: genesis, site binding, \
packs and registry.")
   (provision (list (home-gascity-supervisor-provision config)))
   (one-shot? #t)
   (start #~(lambda args
              (zero? (spawn-command
                      (list #$(gascity-supervisor-provision-program
                               config))))))
   (stop #~(const #f))))

(define (home-gascity-supervisor-shepherd-service config)
  "Return the long-running Home Shepherd service of the supervisor instance
CONFIG: the shared secrets-loading wrapper that execs `gc supervisor run' as
the current user, with no `#:user'/`#:group' and no networking requirement."
  (let ((home (gascity-supervisor-state-directory config)))
    (shepherd-service
     (documentation
      "Run a Gas City supervisor as a user service (gc supervisor run).")
     (provision (list (home-gascity-supervisor-shepherd-service-name config)))
     (requirement (list (home-gascity-supervisor-provision config)))
     (start #~(make-forkexec-constructor
               (list #$(gascity-supervisor-program config))
               #:directory #$home
               #:log-file #$(gascity-supervisor-log-file config)
               #:environment-variables
               #$(gascity-supervisor-environment config)))
     (stop #~(make-kill-destructor))
     (respawn? #t))))

(define (home-gascity-supervisor-shepherd-services value)
  "Return the Home Shepherd services of the supervisor instances of VALUE:
for each instance, the provision one-shot followed by the supervisor, which
requires it."
  (append-map (lambda (config)
                (list (home-gascity-supervisor-provision-shepherd-service
                       config)
                      (home-gascity-supervisor-shepherd-service config)))
              (home-gascity-supervisor-configurations value)))


;;;
;;; Activation and profile.
;;;

(define (home-gascity-supervisor-activation value)
  "Return the Home activation gexp of VALUE: create each instance's state
directory and materialize its generated files for the user that owns the
Home environment.  Unlike the system activation it does not chown and does
not create the service account."
  (let* ((configs (home-gascity-supervisor-configurations value))
         (directories
          (map gascity-supervisor-state-directory configs)))
    #~(begin
        (use-modules (guix build utils))
        (for-each mkdir-p '#$directories)
        #$@(map (lambda (config)
                  #~(invoke
                     #$(gascity-supervisor-activation-program
                        config #:chown? #f)))
                configs))))

(define (home-gascity-supervisor-profile value)
  "Return the packages of the supervisor instances of VALUE."
  (delete-duplicates
   (map gascity-supervisor-configuration-package
        (home-gascity-supervisor-configurations value))))


;;;
;;; Service type.
;;;

(define home-gascity-service-type
  (service-type
   (inherit (system->home-service-type gascity-service-type))
   ;; 'system->home-service-type' alone is not enough: the derived type has no
   ;; mapping for 'account-service-type' (a Home environment has no system
   ;; accounts), and the system Shepherd and activation extensions hardcode a
   ;; dedicated user and /var/... paths.  Redefine the extensions explicitly,
   ;; as github-actions does (@16.4).  The system extensions dropped on
   ;; purpose are 'account-service-type' (no system accounts), the system
   ;; shepherd-root and activation extensions (they run as root and under
   ;; /var), and the system profile extension; they are replaced by the Home
   ;; shepherd, activation and profile extensions below.
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-gascity-supervisor-shepherd-services)
          (service-extension home-activation-service-type
                             home-gascity-supervisor-activation)
          (service-extension home-profile-service-type
                             home-gascity-supervisor-profile)))
   (default-value
     ;; One instance without id, whose state directory, log directory and
     ;; GC_HOME default to the Home paths.
     (list (home-gascity-supervisor-configuration
            (gascity-supervisor-configuration))))
   (description
    "Run Gas City supervisors (gc supervisor run) as user-level Shepherd
services.  This is the Home counterpart of @code{gascity-service-type}: the
value is a list of @code{gascity-supervisor-configuration} records, one per
supervisor process, and the supervisors run as the user that owns the Home
environment.  Each instance has its own @code{gc-home} (the registry,
settings, control socket and instance lock live under it) and its own API
@code{port}, so several supervisors coexist: a lone instance defaults to port
8372, and every instance of a multi-instance service must set @code{port}
explicitly.  The state and log directories default to
@file{$XDG_STATE_HOME/gascity} and @code{gc-home} to @file{$HOME/.gc}.  A
paired one-shot Shepherd service provisions each instance's cities (genesis,
site binding, optional pack fetching and the @file{cities.toml} registry)
before the supervisor starts.  Use @code{gascity-service-config} to configure
the common single-instance case, and pass @code{'()} when only extending a
configured service with further instances.")))

;; Allow a Home configuration that contains a system service configuration
;; for Gas City to be mapped to the Home service, as (gnu home services
;; mcron) does.
(define-service-type-mapping
  gascity-service-type => home-gascity-service-type)
