;;; records.scm --- GENERATED Gas City leaf configuration records
;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of the r0man Guix channel.
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; THIS FILE IS GENERATED.  DO NOT EDIT BY HAND.
;;;
;;; Regenerate with `make generate-gascity-records' and check the
;;; committed copy with `make check-gascity-records'.  The generator is
;;; scripts/generate-gascity-records.scm; its inputs are the committed
;;; copies of the Gas City JSON schemas:
;;;
;;;   city-schema.json  $id: https://github.com/gastownhall/gascity/internal/config/city
;;;                     $schema: https://json-schema.org/draft/2020-12/schema
;;;                     title: Gas City Configuration
;;;   pack-schema.json  $id: https://github.com/gastownhall/gascity/internal/config/pack-config
;;;                     $schema: https://json-schema.org/draft/2020-12/schema
;;;                     title: Gas City Pack Manifest
;;;
;;; This is a standalone leaf-record surface: it imports only
;;; `(guix records)', `(guix gexp)', `(r0man guix toml)',
;;; `(srfi srfi-1)' and `(ice-9 match)', and inlines its own
;;; serialization, so it never depends on
;;; `(r0man guix services gascity)'.  Every field defaults to
;;; `unset'; a record's `->toml' returns the ordered list of TOML nodes
;;; for its body, in field-declaration order.
;;;
;;; Code:

(define-module (r0man guix services gascity records)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (r0man guix toml)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (
            gascity-acp-session-configuration
            gascity-acp-session-configuration->toml
            gascity-acp-session-configuration-handshake-timeout
            gascity-acp-session-configuration-nudge-busy-timeout
            gascity-acp-session-configuration-output-buffer-lines
            gascity-acp-session-configuration-stop-grace
            gascity-acp-session-configuration?
            gascity-agent-configuration
            gascity-agent-configuration->toml
            gascity-agent-configuration-append-fragments
            gascity-agent-configuration-args
            gascity-agent-configuration-assigned-work-defer-limit
            gascity-agent-configuration-attach
            gascity-agent-configuration-auto-reclaim-stale-claims
            gascity-agent-configuration-context-advisory
            gascity-agent-configuration-default-sling-formula
            gascity-agent-configuration-depends-on
            gascity-agent-configuration-description
            gascity-agent-configuration-dir
            gascity-agent-configuration-drain-timeout
            gascity-agent-configuration-emits-permission-warning
            gascity-agent-configuration-env
            gascity-agent-configuration-hooks-installed
            gascity-agent-configuration-idle-timeout
            gascity-agent-configuration-inject-assigned-skills
            gascity-agent-configuration-inject-fragments
            gascity-agent-configuration-install-agent-hooks
            gascity-agent-configuration-lifecycle
            gascity-agent-configuration-max-active-sessions
            gascity-agent-configuration-max-session-age
            gascity-agent-configuration-max-session-age-jitter
            gascity-agent-configuration-mcp
            gascity-agent-configuration-min-active-sessions
            gascity-agent-configuration-mouse-mode
            gascity-agent-configuration-name
            gascity-agent-configuration-namepool
            gascity-agent-configuration-nudge
            gascity-agent-configuration-on-boot
            gascity-agent-configuration-on-death
            gascity-agent-configuration-option-defaults
            gascity-agent-configuration-overlay-dir
            gascity-agent-configuration-pre-start
            gascity-agent-configuration-process-names
            gascity-agent-configuration-prompt-flag
            gascity-agent-configuration-prompt-mode
            gascity-agent-configuration-prompt-template
            gascity-agent-configuration-provider
            gascity-agent-configuration-ready-delay-ms
            gascity-agent-configuration-ready-prompt-prefix
            gascity-agent-configuration-resume-command
            gascity-agent-configuration-scale-check
            gascity-agent-configuration-scope
            gascity-agent-configuration-session
            gascity-agent-configuration-session-live
            gascity-agent-configuration-session-setup
            gascity-agent-configuration-session-setup-script
            gascity-agent-configuration-skills
            gascity-agent-configuration-sleep-after-idle
            gascity-agent-configuration-sling-query
            gascity-agent-configuration-start-command
            gascity-agent-configuration-suspended
            gascity-agent-configuration-tmux-alias
            gascity-agent-configuration-upstream
            gascity-agent-configuration-wake-mode
            gascity-agent-configuration-work-dir
            gascity-agent-configuration-work-query
            gascity-agent-configuration?
            gascity-agent-defaults-configuration
            gascity-agent-defaults-configuration->toml
            gascity-agent-defaults-configuration-allow-env-override
            gascity-agent-defaults-configuration-allow-overlay
            gascity-agent-defaults-configuration-append-fragments
            gascity-agent-defaults-configuration-context-advisory
            gascity-agent-defaults-configuration-default-sling-formula
            gascity-agent-defaults-configuration-mcp
            gascity-agent-defaults-configuration-model
            gascity-agent-defaults-configuration-provider
            gascity-agent-defaults-configuration-skills
            gascity-agent-defaults-configuration-upstream
            gascity-agent-defaults-configuration-wake-mode
            gascity-agent-defaults-configuration?
            gascity-agent-override-configuration
            gascity-agent-override-configuration->toml
            gascity-agent-override-configuration-agent
            gascity-agent-override-configuration-append-fragments
            gascity-agent-override-configuration-args
            gascity-agent-override-configuration-assigned-work-defer-limit
            gascity-agent-override-configuration-attach
            gascity-agent-override-configuration-auto-reclaim-stale-claims
            gascity-agent-override-configuration-context-advisory
            gascity-agent-override-configuration-default-sling-formula
            gascity-agent-override-configuration-depends-on
            gascity-agent-override-configuration-dir
            gascity-agent-override-configuration-env
            gascity-agent-override-configuration-env-remove
            gascity-agent-override-configuration-hooks-installed
            gascity-agent-override-configuration-idle-timeout
            gascity-agent-override-configuration-inject-assigned-skills
            gascity-agent-override-configuration-inject-fragments
            gascity-agent-override-configuration-inject-fragments-append
            gascity-agent-override-configuration-install-agent-hooks
            gascity-agent-override-configuration-install-agent-hooks-append
            gascity-agent-override-configuration-lifecycle
            gascity-agent-override-configuration-max-active-sessions
            gascity-agent-override-configuration-max-session-age
            gascity-agent-override-configuration-max-session-age-jitter
            gascity-agent-override-configuration-mcp
            gascity-agent-override-configuration-mcp-append
            gascity-agent-override-configuration-min-active-sessions
            gascity-agent-override-configuration-mouse-mode
            gascity-agent-override-configuration-nudge
            gascity-agent-override-configuration-option-defaults
            gascity-agent-override-configuration-overlay-dir
            gascity-agent-override-configuration-pool
            gascity-agent-override-configuration-pre-start
            gascity-agent-override-configuration-pre-start-append
            gascity-agent-override-configuration-prompt-template
            gascity-agent-override-configuration-provider
            gascity-agent-override-configuration-resume-command
            gascity-agent-override-configuration-scale-check
            gascity-agent-override-configuration-scope
            gascity-agent-override-configuration-session
            gascity-agent-override-configuration-session-live
            gascity-agent-override-configuration-session-live-append
            gascity-agent-override-configuration-session-setup
            gascity-agent-override-configuration-session-setup-append
            gascity-agent-override-configuration-session-setup-script
            gascity-agent-override-configuration-skills
            gascity-agent-override-configuration-skills-append
            gascity-agent-override-configuration-sleep-after-idle
            gascity-agent-override-configuration-start-command
            gascity-agent-override-configuration-suspended
            gascity-agent-override-configuration-tmux-alias
            gascity-agent-override-configuration-upstream
            gascity-agent-override-configuration-wake-mode
            gascity-agent-override-configuration-work-dir
            gascity-agent-override-configuration?
            gascity-agent-patch-configuration
            gascity-agent-patch-configuration->toml
            gascity-agent-patch-configuration-append-fragments
            gascity-agent-patch-configuration-args
            gascity-agent-patch-configuration-assigned-work-defer-limit
            gascity-agent-patch-configuration-attach
            gascity-agent-patch-configuration-auto-reclaim-stale-claims
            gascity-agent-patch-configuration-context-advisory
            gascity-agent-patch-configuration-default-sling-formula
            gascity-agent-patch-configuration-depends-on
            gascity-agent-patch-configuration-dir
            gascity-agent-patch-configuration-env
            gascity-agent-patch-configuration-env-remove
            gascity-agent-patch-configuration-hooks-installed
            gascity-agent-patch-configuration-idle-timeout
            gascity-agent-patch-configuration-inject-assigned-skills
            gascity-agent-patch-configuration-inject-fragments
            gascity-agent-patch-configuration-inject-fragments-append
            gascity-agent-patch-configuration-install-agent-hooks
            gascity-agent-patch-configuration-install-agent-hooks-append
            gascity-agent-patch-configuration-lifecycle
            gascity-agent-patch-configuration-max-active-sessions
            gascity-agent-patch-configuration-max-session-age
            gascity-agent-patch-configuration-max-session-age-jitter
            gascity-agent-patch-configuration-mcp
            gascity-agent-patch-configuration-mcp-append
            gascity-agent-patch-configuration-min-active-sessions
            gascity-agent-patch-configuration-mouse-mode
            gascity-agent-patch-configuration-name
            gascity-agent-patch-configuration-nudge
            gascity-agent-patch-configuration-option-defaults
            gascity-agent-patch-configuration-overlay-dir
            gascity-agent-patch-configuration-pool
            gascity-agent-patch-configuration-pre-start
            gascity-agent-patch-configuration-pre-start-append
            gascity-agent-patch-configuration-prompt-template
            gascity-agent-patch-configuration-provider
            gascity-agent-patch-configuration-resume-command
            gascity-agent-patch-configuration-rig
            gascity-agent-patch-configuration-scale-check
            gascity-agent-patch-configuration-scope
            gascity-agent-patch-configuration-session
            gascity-agent-patch-configuration-session-live
            gascity-agent-patch-configuration-session-live-append
            gascity-agent-patch-configuration-session-setup
            gascity-agent-patch-configuration-session-setup-append
            gascity-agent-patch-configuration-session-setup-script
            gascity-agent-patch-configuration-skills
            gascity-agent-patch-configuration-skills-append
            gascity-agent-patch-configuration-sleep-after-idle
            gascity-agent-patch-configuration-start-command
            gascity-agent-patch-configuration-suspended
            gascity-agent-patch-configuration-tmux-alias
            gascity-agent-patch-configuration-upstream
            gascity-agent-patch-configuration-wake-mode
            gascity-agent-patch-configuration-work-dir
            gascity-agent-patch-configuration?
            gascity-api-configuration
            gascity-api-configuration->toml
            gascity-api-configuration-allow-mutations
            gascity-api-configuration-bind
            gascity-api-configuration-port
            gascity-api-configuration-read-auth-required
            gascity-api-configuration-read-auth-verify-key
            gascity-api-configuration-write-auth-allow-unverified
            gascity-api-configuration-write-auth-required
            gascity-api-configuration-write-auth-verify-key
            gascity-api-configuration?
            gascity-bead-policy-configuration
            gascity-bead-policy-configuration->toml
            gascity-bead-policy-configuration-delete-after-close
            gascity-bead-policy-configuration-storage
            gascity-bead-policy-configuration?
            gascity-beads-configuration
            gascity-beads-configuration->toml
            gascity-beads-configuration-backend
            gascity-beads-configuration-bd-compatibility
            gascity-beads-configuration-conditional-writes
            gascity-beads-configuration-event-hooks
            gascity-beads-configuration-guarded-release
            gascity-beads-configuration-policies
            gascity-beads-configuration-provider
            gascity-beads-configuration?
            gascity-chat-sessions-configuration
            gascity-chat-sessions-configuration->toml
            gascity-chat-sessions-configuration-grace-period
            gascity-chat-sessions-configuration-idle-timeout
            gascity-chat-sessions-configuration?
            gascity-city-configuration
            gascity-city-configuration->toml
            gascity-city-configuration-agent
            gascity-city-configuration-agent-defaults
            gascity-city-configuration-api
            gascity-city-configuration-beads
            gascity-city-configuration-chat-sessions
            gascity-city-configuration-convergence
            gascity-city-configuration-daemon
            gascity-city-configuration-defaults
            gascity-city-configuration-doctor
            gascity-city-configuration-dolt
            gascity-city-configuration-events
            gascity-city-configuration-extmsg
            gascity-city-configuration-formulas
            gascity-city-configuration-github
            gascity-city-configuration-imports
            gascity-city-configuration-include
            gascity-city-configuration-mail
            gascity-city-configuration-maintenance
            gascity-city-configuration-named-session
            gascity-city-configuration-orders
            gascity-city-configuration-patches
            gascity-city-configuration-pricing
            gascity-city-configuration-providers
            gascity-city-configuration-rigs
            gascity-city-configuration-service
            gascity-city-configuration-session
            gascity-city-configuration-session-sleep
            gascity-city-configuration-storage
            gascity-city-configuration-upstreams
            gascity-city-configuration-usage
            gascity-city-configuration-webhook
            gascity-city-configuration-webhooks
            gascity-city-configuration-workspace
            gascity-city-configuration?
            gascity-context-advisory-configuration
            gascity-context-advisory-configuration->toml
            gascity-context-advisory-configuration-enabled
            gascity-context-advisory-configuration-tiers
            gascity-context-advisory-configuration-window-tokens
            gascity-context-advisory-configuration?
            gascity-context-advisory-tier-configuration
            gascity-context-advisory-tier-configuration->toml
            gascity-context-advisory-tier-configuration-enabled
            gascity-context-advisory-tier-configuration-message
            gascity-context-advisory-tier-configuration-threshold
            gascity-context-advisory-tier-configuration?
            gascity-convergence-configuration
            gascity-convergence-configuration->toml
            gascity-convergence-configuration-max-per-agent
            gascity-convergence-configuration-max-total
            gascity-convergence-configuration?
            gascity-daemon-configuration
            gascity-daemon-configuration->toml
            gascity-daemon-configuration-auto-prune-worker-dir
            gascity-daemon-configuration-auto-reap-closed-bead-worktrees
            gascity-daemon-configuration-auto-reap-closed-bead-worktrees-dry-run
            gascity-daemon-configuration-auto-reap-closed-bead-worktrees-min-age-minutes
            gascity-daemon-configuration-auto-restart-on-drift
            gascity-daemon-configuration-dolt-start-address-in-use-retry-window
            gascity-daemon-configuration-dolt-stop-timeout
            gascity-daemon-configuration-drift-drain-timeout
            gascity-daemon-configuration-formula-v2
            gascity-daemon-configuration-graph-workflows
            gascity-daemon-configuration-max-restarts
            gascity-daemon-configuration-max-wakes-per-tick
            gascity-daemon-configuration-nudge-dispatcher
            gascity-daemon-configuration-observe-paths
            gascity-daemon-configuration-patrol-interval
            gascity-daemon-configuration-probe-concurrency
            gascity-daemon-configuration-restart-window
            gascity-daemon-configuration-session-circuit-breaker
            gascity-daemon-configuration-session-circuit-breaker-max-restarts
            gascity-daemon-configuration-session-circuit-breaker-reset-after
            gascity-daemon-configuration-session-circuit-breaker-window
            gascity-daemon-configuration-shutdown-timeout
            gascity-daemon-configuration-start-ready-timeout
            gascity-daemon-configuration-tick-debounce
            gascity-daemon-configuration-wisp-gc-interval
            gascity-daemon-configuration-wisp-ttl
            gascity-daemon-configuration?
            gascity-doctor-configuration
            gascity-doctor-configuration->toml
            gascity-doctor-configuration-check
            gascity-doctor-configuration-nested-worktree-prune
            gascity-doctor-configuration-worktree-rig-error-size
            gascity-doctor-configuration-worktree-rig-warn-size
            gascity-doctor-configuration?
            gascity-dolt-configuration
            gascity-dolt-configuration->toml
            gascity-dolt-configuration-archive-level
            gascity-dolt-configuration-auto-gc-enabled
            gascity-dolt-configuration-dolt-lock-release-timeout
            gascity-dolt-configuration-host
            gascity-dolt-configuration-max-connections
            gascity-dolt-configuration-port
            gascity-dolt-configuration-read-timeout-millis
            gascity-dolt-configuration-wait-timeout-seconds
            gascity-dolt-configuration-write-timeout-millis
            gascity-dolt-configuration?
            gascity-dolt-maintenance-configuration
            gascity-dolt-maintenance-configuration->toml
            gascity-dolt-maintenance-configuration-alert-to
            gascity-dolt-maintenance-configuration-enabled
            gascity-dolt-maintenance-configuration-gc-timeout
            gascity-dolt-maintenance-configuration-interval
            gascity-dolt-maintenance-configuration?
            gascity-events-configuration
            gascity-events-configuration->toml
            gascity-events-configuration-provider
            gascity-events-configuration-rotation
            gascity-events-configuration?
            gascity-events-rotation-configuration
            gascity-events-rotation-configuration->toml
            gascity-events-rotation-configuration-archive-retain-age
            gascity-events-rotation-configuration-check-interval-records
            gascity-events-rotation-configuration-check-interval-seconds
            gascity-events-rotation-configuration-enabled
            gascity-events-rotation-configuration-max-size-bytes
            gascity-events-rotation-configuration?
            gascity-ext-msg-configuration
            gascity-ext-msg-configuration->toml
            gascity-ext-msg-configuration-default-route
            gascity-ext-msg-configuration?
            gascity-ext-msg-default-route-configuration
            gascity-ext-msg-default-route-configuration->toml
            gascity-ext-msg-default-route-configuration-account-id
            gascity-ext-msg-default-route-configuration-agent
            gascity-ext-msg-default-route-configuration-provider
            gascity-ext-msg-default-route-configuration?
            gascity-formulas-configuration
            gascity-formulas-configuration->toml
            gascity-formulas-configuration?
            gascity-github-configuration
            gascity-github-configuration->toml
            gascity-github-configuration-pr-monitor
            gascity-github-configuration?
            gascity-github-pr-monitor-configuration
            gascity-github-pr-monitor-configuration->toml
            gascity-github-pr-monitor-configuration-base-branches
            gascity-github-pr-monitor-configuration-merge-queue
            gascity-github-pr-monitor-configuration-name
            gascity-github-pr-monitor-configuration-notify
            gascity-github-pr-monitor-configuration-owner
            gascity-github-pr-monitor-configuration-poll-interval
            gascity-github-pr-monitor-configuration-repair-route
            gascity-github-pr-monitor-configuration-repair-workflow
            gascity-github-pr-monitor-configuration-repo
            gascity-github-pr-monitor-configuration-rig
            gascity-github-pr-monitor-configuration-webhook-secret-env
            gascity-github-pr-monitor-configuration-webhook-secret-key
            gascity-github-pr-monitor-configuration?
            gascity-github-pr-monitor-patch-configuration
            gascity-github-pr-monitor-patch-configuration->toml
            gascity-github-pr-monitor-patch-configuration-base-branches
            gascity-github-pr-monitor-patch-configuration-merge-queue
            gascity-github-pr-monitor-patch-configuration-name
            gascity-github-pr-monitor-patch-configuration-notify
            gascity-github-pr-monitor-patch-configuration-notify-append
            gascity-github-pr-monitor-patch-configuration-owner
            gascity-github-pr-monitor-patch-configuration-poll-interval
            gascity-github-pr-monitor-patch-configuration-repair-route
            gascity-github-pr-monitor-patch-configuration-repair-workflow
            gascity-github-pr-monitor-patch-configuration-repo
            gascity-github-pr-monitor-patch-configuration-rig
            gascity-github-pr-monitor-patch-configuration-webhook-secret-env
            gascity-github-pr-monitor-patch-configuration-webhook-secret-key
            gascity-github-pr-monitor-patch-configuration?
            gascity-import-configuration
            gascity-import-configuration->toml
            gascity-import-configuration-source
            gascity-import-configuration-version
            gascity-import-configuration?
            gascity-k8s-configuration
            gascity-k8s-configuration->toml
            gascity-k8s-configuration-context
            gascity-k8s-configuration-cpu-limit
            gascity-k8s-configuration-cpu-request
            gascity-k8s-configuration-image
            gascity-k8s-configuration-mem-limit
            gascity-k8s-configuration-mem-request
            gascity-k8s-configuration-namespace
            gascity-k8s-configuration-prebaked
            gascity-k8s-configuration?
            gascity-local-doctor-check-configuration
            gascity-local-doctor-check-configuration->toml
            gascity-local-doctor-check-configuration-description
            gascity-local-doctor-check-configuration-fix
            gascity-local-doctor-check-configuration-name
            gascity-local-doctor-check-configuration-script
            gascity-local-doctor-check-configuration?
            gascity-mail-configuration
            gascity-mail-configuration->toml
            gascity-mail-configuration-provider
            gascity-mail-configuration-retention-ttl
            gascity-mail-configuration?
            gascity-maintenance-configuration
            gascity-maintenance-configuration->toml
            gascity-maintenance-configuration-dolt
            gascity-maintenance-configuration?
            gascity-model-pricing-configuration
            gascity-model-pricing-configuration->toml
            gascity-model-pricing-configuration-last-verified
            gascity-model-pricing-configuration-model
            gascity-model-pricing-configuration-provider
            gascity-model-pricing-configuration-tier
            gascity-model-pricing-configuration?
            gascity-named-session-configuration
            gascity-named-session-configuration->toml
            gascity-named-session-configuration-dir
            gascity-named-session-configuration-mode
            gascity-named-session-configuration-name
            gascity-named-session-configuration-scope
            gascity-named-session-configuration-template
            gascity-named-session-configuration?
            gascity-named-session-patch-configuration
            gascity-named-session-patch-configuration->toml
            gascity-named-session-patch-configuration-dir
            gascity-named-session-patch-configuration-mode
            gascity-named-session-patch-configuration-name
            gascity-named-session-patch-configuration-template
            gascity-named-session-patch-configuration?
            gascity-option-choice-configuration
            gascity-option-choice-configuration->toml
            gascity-option-choice-configuration-flag-aliases
            gascity-option-choice-configuration-flag-args
            gascity-option-choice-configuration-label
            gascity-option-choice-configuration-value
            gascity-option-choice-configuration?
            gascity-order-override-configuration
            gascity-order-override-configuration->toml
            gascity-order-override-configuration-check
            gascity-order-override-configuration-check-timeout
            gascity-order-override-configuration-enabled
            gascity-order-override-configuration-env
            gascity-order-override-configuration-gate
            gascity-order-override-configuration-idempotent
            gascity-order-override-configuration-interval
            gascity-order-override-configuration-name
            gascity-order-override-configuration-on
            gascity-order-override-configuration-pool
            gascity-order-override-configuration-rig
            gascity-order-override-configuration-schedule
            gascity-order-override-configuration-timeout
            gascity-order-override-configuration-trigger
            gascity-order-override-configuration?
            gascity-orders-configuration
            gascity-orders-configuration->toml
            gascity-orders-configuration-max-dispatches-per-tick
            gascity-orders-configuration-max-timeout
            gascity-orders-configuration-overrides
            gascity-orders-configuration-skip
            gascity-orders-configuration?
            gascity-pack-command-entry-configuration
            gascity-pack-command-entry-configuration->toml
            gascity-pack-command-entry-configuration-description
            gascity-pack-command-entry-configuration-long-description
            gascity-pack-command-entry-configuration-name
            gascity-pack-command-entry-configuration-script
            gascity-pack-command-entry-configuration?
            gascity-pack-configuration
            gascity-pack-configuration->toml
            gascity-pack-configuration-agent
            gascity-pack-configuration-agent-defaults
            gascity-pack-configuration-commands
            gascity-pack-configuration-doctor
            gascity-pack-configuration-global
            gascity-pack-configuration-imports
            gascity-pack-configuration-named-session
            gascity-pack-configuration-pack
            gascity-pack-configuration-patches
            gascity-pack-configuration-pricing
            gascity-pack-configuration-providers
            gascity-pack-configuration-runtimes
            gascity-pack-configuration-service
            gascity-pack-configuration-upstreams
            gascity-pack-configuration-webhook
            gascity-pack-configuration?
            gascity-pack-defaults-configuration
            gascity-pack-defaults-configuration->toml
            gascity-pack-defaults-configuration-rig
            gascity-pack-defaults-configuration?
            gascity-pack-doctor-entry-configuration
            gascity-pack-doctor-entry-configuration->toml
            gascity-pack-doctor-entry-configuration-description
            gascity-pack-doctor-entry-configuration-fix
            gascity-pack-doctor-entry-configuration-name
            gascity-pack-doctor-entry-configuration-script
            gascity-pack-doctor-entry-configuration-warmup
            gascity-pack-doctor-entry-configuration?
            gascity-pack-global-configuration
            gascity-pack-global-configuration->toml
            gascity-pack-global-configuration-session-live
            gascity-pack-global-configuration?
            gascity-pack-meta-configuration
            gascity-pack-meta-configuration->toml
            gascity-pack-meta-configuration-description
            gascity-pack-meta-configuration-includes
            gascity-pack-meta-configuration-name
            gascity-pack-meta-configuration-requires
            gascity-pack-meta-configuration-requires-gc
            gascity-pack-meta-configuration-schema
            gascity-pack-meta-configuration-version
            gascity-pack-meta-configuration?
            gascity-pack-patches-configuration
            gascity-pack-patches-configuration->toml
            gascity-pack-patches-configuration-agent
            gascity-pack-patches-configuration?
            gascity-pack-requirement-configuration
            gascity-pack-requirement-configuration->toml
            gascity-pack-requirement-configuration-agent
            gascity-pack-requirement-configuration-scope
            gascity-pack-requirement-configuration?
            gascity-pack-rig-defaults-configuration
            gascity-pack-rig-defaults-configuration->toml
            gascity-pack-rig-defaults-configuration-imports
            gascity-pack-rig-defaults-configuration?
            gascity-pack-runtime-entry-configuration
            gascity-pack-runtime-entry-configuration->toml
            gascity-pack-runtime-entry-configuration-command
            gascity-pack-runtime-entry-configuration-prompt-delivery
            gascity-pack-runtime-entry-configuration-protocol
            gascity-pack-runtime-entry-configuration?
            gascity-patches-configuration
            gascity-patches-configuration->toml
            gascity-patches-configuration-agent
            gascity-patches-configuration-github-pr-monitor
            gascity-patches-configuration-named-session
            gascity-patches-configuration-providers
            gascity-patches-configuration-rigs
            gascity-patches-configuration?
            gascity-pool-override-configuration
            gascity-pool-override-configuration->toml
            gascity-pool-override-configuration-check
            gascity-pool-override-configuration-drain-timeout
            gascity-pool-override-configuration-max
            gascity-pool-override-configuration-min
            gascity-pool-override-configuration-on-boot
            gascity-pool-override-configuration-on-death
            gascity-pool-override-configuration?
            gascity-provider-option-configuration
            gascity-provider-option-configuration->toml
            gascity-provider-option-configuration-choices
            gascity-provider-option-configuration-default
            gascity-provider-option-configuration-flag-template
            gascity-provider-option-configuration-key
            gascity-provider-option-configuration-label
            gascity-provider-option-configuration-omit
            gascity-provider-option-configuration-type
            gascity-provider-option-configuration?
            gascity-provider-patch-configuration
            gascity-provider-patch-configuration--replace
            gascity-provider-patch-configuration->toml
            gascity-provider-patch-configuration-accept-startup-dialogs
            gascity-provider-patch-configuration-acp-args
            gascity-provider-patch-configuration-acp-command
            gascity-provider-patch-configuration-args
            gascity-provider-patch-configuration-args-append
            gascity-provider-patch-configuration-base
            gascity-provider-patch-configuration-command
            gascity-provider-patch-configuration-env
            gascity-provider-patch-configuration-env-remove
            gascity-provider-patch-configuration-name
            gascity-provider-patch-configuration-options-schema-merge
            gascity-provider-patch-configuration-prompt-flag
            gascity-provider-patch-configuration-prompt-mode
            gascity-provider-patch-configuration-ready-delay-ms
            gascity-provider-patch-configuration?
            gascity-provider-spec-configuration
            gascity-provider-spec-configuration->toml
            gascity-provider-spec-configuration-accept-startup-dialogs
            gascity-provider-spec-configuration-acp-args
            gascity-provider-spec-configuration-acp-command
            gascity-provider-spec-configuration-args
            gascity-provider-spec-configuration-args-append
            gascity-provider-spec-configuration-base
            gascity-provider-spec-configuration-command
            gascity-provider-spec-configuration-display-name
            gascity-provider-spec-configuration-emits-permission-warning
            gascity-provider-spec-configuration-env
            gascity-provider-spec-configuration-fork-flag
            gascity-provider-spec-configuration-instructions-file
            gascity-provider-spec-configuration-option-defaults
            gascity-provider-spec-configuration-options-schema
            gascity-provider-spec-configuration-options-schema-merge
            gascity-provider-spec-configuration-path-check
            gascity-provider-spec-configuration-permission-modes
            gascity-provider-spec-configuration-print-args
            gascity-provider-spec-configuration-process-names
            gascity-provider-spec-configuration-prompt-flag
            gascity-provider-spec-configuration-prompt-mode
            gascity-provider-spec-configuration-ready-delay-ms
            gascity-provider-spec-configuration-ready-prompt-prefix
            gascity-provider-spec-configuration-resume-command
            gascity-provider-spec-configuration-resume-flag
            gascity-provider-spec-configuration-resume-style
            gascity-provider-spec-configuration-session-id-flag
            gascity-provider-spec-configuration-supports-acp
            gascity-provider-spec-configuration-supports-hooks
            gascity-provider-spec-configuration-title-model
            gascity-provider-spec-configuration-upstream-env
            gascity-provider-spec-configuration?
            gascity-rig-configuration
            gascity-rig-configuration->toml
            gascity-rig-configuration-default-branch
            gascity-rig-configuration-default-sling-target
            gascity-rig-configuration-default-sling-targets
            gascity-rig-configuration-dolt-host
            gascity-rig-configuration-dolt-port
            gascity-rig-configuration-formula-vars
            gascity-rig-configuration-formulas-dir
            gascity-rig-configuration-imports
            gascity-rig-configuration-includes
            gascity-rig-configuration-max-active-sessions
            gascity-rig-configuration-name
            gascity-rig-configuration-overrides
            gascity-rig-configuration-patches
            gascity-rig-configuration-path
            gascity-rig-configuration-prefix
            gascity-rig-configuration-session-sleep
            gascity-rig-configuration-suspended
            gascity-rig-configuration-suspended-on-start
            gascity-rig-configuration?
            gascity-rig-patch-configuration
            gascity-rig-patch-configuration->toml
            gascity-rig-patch-configuration-default-branch
            gascity-rig-patch-configuration-formula-vars
            gascity-rig-patch-configuration-name
            gascity-rig-patch-configuration-path
            gascity-rig-patch-configuration-prefix
            gascity-rig-patch-configuration-suspended
            gascity-rig-patch-configuration-suspended-on-start
            gascity-rig-patch-configuration?
            gascity-service-configuration
            gascity-service-configuration->toml
            gascity-service-configuration-kind
            gascity-service-configuration-name
            gascity-service-configuration-process
            gascity-service-configuration-publication
            gascity-service-configuration-publish-mode
            gascity-service-configuration-state-root
            gascity-service-configuration-workflow
            gascity-service-configuration?
            gascity-service-process-configuration
            gascity-service-process-configuration->toml
            gascity-service-process-configuration-command
            gascity-service-process-configuration-health-path
            gascity-service-process-configuration?
            gascity-service-publication-configuration
            gascity-service-publication-configuration->toml
            gascity-service-publication-configuration-allow-websockets
            gascity-service-publication-configuration-hostname
            gascity-service-publication-configuration-visibility
            gascity-service-publication-configuration?
            gascity-service-workflow-configuration
            gascity-service-workflow-configuration->toml
            gascity-service-workflow-configuration-contract
            gascity-service-workflow-configuration?
            gascity-session-configuration
            gascity-session-configuration->toml
            gascity-session-configuration-acp
            gascity-session-configuration-claim-holder-stall-timeout
            gascity-session-configuration-debounce-ms
            gascity-session-configuration-display-ms
            gascity-session-configuration-k8s
            gascity-session-configuration-nudge-lock-timeout
            gascity-session-configuration-nudge-poll-interval
            gascity-session-configuration-nudge-ready-timeout
            gascity-session-configuration-nudge-retry-interval
            gascity-session-configuration-progress-stall-timeout
            gascity-session-configuration-provider
            gascity-session-configuration-remote-match
            gascity-session-configuration-setup-max-timeout
            gascity-session-configuration-setup-timeout
            gascity-session-configuration-socket
            gascity-session-configuration-startup-timeout
            gascity-session-configuration?
            gascity-session-sleep-configuration
            gascity-session-sleep-configuration->toml
            gascity-session-sleep-configuration-interactive-fresh
            gascity-session-sleep-configuration-interactive-resume
            gascity-session-sleep-configuration-noninteractive
            gascity-session-sleep-configuration?
            gascity-storage-binding-configuration
            gascity-storage-binding-configuration->toml
            gascity-storage-binding-configuration-auth
            gascity-storage-binding-configuration-config-ref
            gascity-storage-binding-configuration-path
            gascity-storage-binding-configuration-provider
            gascity-storage-binding-configuration-url
            gascity-storage-binding-configuration?
            gascity-storage-classes-configuration
            gascity-storage-classes-configuration->toml
            gascity-storage-classes-configuration-graph
            gascity-storage-classes-configuration-messaging
            gascity-storage-classes-configuration-nudges
            gascity-storage-classes-configuration-orders
            gascity-storage-classes-configuration-sessions
            gascity-storage-classes-configuration-work
            gascity-storage-classes-configuration?
            gascity-storage-configuration
            gascity-storage-configuration->toml
            gascity-storage-configuration-bindings
            gascity-storage-configuration-classes
            gascity-storage-configuration?
            gascity-tier-configuration
            gascity-tier-configuration->toml
            gascity-tier-configuration-cache-creation-usd-per-1m
            gascity-tier-configuration-cache-read-usd-per-1m
            gascity-tier-configuration-completion-usd-per-1m
            gascity-tier-configuration-prompt-usd-per-1m
            gascity-tier-configuration?
            gascity-upstream-env-binding-configuration
            gascity-upstream-env-binding-configuration->toml
            gascity-upstream-env-binding-configuration-api-key
            gascity-upstream-env-binding-configuration-auth-token
            gascity-upstream-env-binding-configuration-base-url
            gascity-upstream-env-binding-configuration?
            gascity-upstream-spec-configuration
            gascity-upstream-spec-configuration->toml
            gascity-upstream-spec-configuration-api-key
            gascity-upstream-spec-configuration-api-key-env
            gascity-upstream-spec-configuration-auth-token
            gascity-upstream-spec-configuration-auth-token-env
            gascity-upstream-spec-configuration-base-url
            gascity-upstream-spec-configuration-base-url-env
            gascity-upstream-spec-configuration-description
            gascity-upstream-spec-configuration-env
            gascity-upstream-spec-configuration?
            gascity-usage-configuration
            gascity-usage-configuration->toml
            gascity-usage-configuration-provider
            gascity-usage-configuration?
            gascity-webhook-allow-public-configuration
            gascity-webhook-allow-public-configuration->toml
            gascity-webhook-allow-public-configuration-digest
            gascity-webhook-allow-public-configuration-name
            gascity-webhook-allow-public-configuration-source
            gascity-webhook-allow-public-configuration?
            gascity-webhook-configuration
            gascity-webhook-configuration->toml
            gascity-webhook-configuration-max-per-minute
            gascity-webhook-configuration-name
            gascity-webhook-configuration-publication
            gascity-webhook-configuration-rig
            gascity-webhook-configuration-rule
            gascity-webhook-configuration-scope
            gascity-webhook-configuration-verify
            gascity-webhook-configuration?
            gascity-webhook-jwt-policy-configuration
            gascity-webhook-jwt-policy-configuration->toml
            gascity-webhook-jwt-policy-configuration-audience
            gascity-webhook-jwt-policy-configuration-issuer
            gascity-webhook-jwt-policy-configuration-jwks-url
            gascity-webhook-jwt-policy-configuration-name
            gascity-webhook-jwt-policy-configuration?
            gascity-webhook-policy-configuration
            gascity-webhook-policy-configuration->toml
            gascity-webhook-policy-configuration-allow-public
            gascity-webhook-policy-configuration-jwt-policy
            gascity-webhook-policy-configuration-rate-limit
            gascity-webhook-policy-configuration?
            gascity-webhook-rate-limit-configuration
            gascity-webhook-rate-limit-configuration->toml
            gascity-webhook-rate-limit-configuration-burst
            gascity-webhook-rate-limit-configuration-override
            gascity-webhook-rate-limit-configuration-per-minute
            gascity-webhook-rate-limit-configuration?
            gascity-webhook-rate-limit-override-configuration
            gascity-webhook-rate-limit-override-configuration->toml
            gascity-webhook-rate-limit-override-configuration-burst
            gascity-webhook-rate-limit-override-configuration-name
            gascity-webhook-rate-limit-override-configuration-per-minute
            gascity-webhook-rate-limit-override-configuration?
            gascity-webhook-rule-configuration
            gascity-webhook-rule-configuration->toml
            gascity-webhook-rule-configuration-args
            gascity-webhook-rule-configuration-event
            gascity-webhook-rule-configuration-match
            gascity-webhook-rule-configuration-order
            gascity-webhook-rule-configuration-rig
            gascity-webhook-rule-configuration-target
            gascity-webhook-rule-configuration?
            gascity-webhook-verify-configuration
            gascity-webhook-verify-configuration->toml
            gascity-webhook-verify-configuration-allowed-cidrs
            gascity-webhook-verify-configuration-audience
            gascity-webhook-verify-configuration-bearer-env
            gascity-webhook-verify-configuration-dedup-header
            gascity-webhook-verify-configuration-event-header
            gascity-webhook-verify-configuration-issuer
            gascity-webhook-verify-configuration-jwks-url
            gascity-webhook-verify-configuration-replay-window
            gascity-webhook-verify-configuration-scheme
            gascity-webhook-verify-configuration-secret-env
            gascity-webhook-verify-configuration-secret-key
            gascity-webhook-verify-configuration-signature-header
            gascity-webhook-verify-configuration-timestamp-header
            gascity-webhook-verify-configuration?
            gascity-workspace-configuration
            gascity-workspace-configuration->toml
            gascity-workspace-configuration-default-rig-includes
            gascity-workspace-configuration-env
            gascity-workspace-configuration-global-fragments
            gascity-workspace-configuration-includes
            gascity-workspace-configuration-install-agent-hooks
            gascity-workspace-configuration-max-active-sessions
            gascity-workspace-configuration-name
            gascity-workspace-configuration-prefix
            gascity-workspace-configuration-provider
            gascity-workspace-configuration-session-template
            gascity-workspace-configuration-start-command
            gascity-workspace-configuration-suspended
            gascity-workspace-configuration-suspended-on-start
            gascity-workspace-configuration-timezone
            gascity-workspace-configuration?
            ))


;;;
;;; Shared serializer helpers.
;;;

;; Every helper returns an ordered list of TOML IR nodes for one field, or the
;; empty list when the field is absent (`unset'), so the record serializers can
;; append the results in declaration order.  A gexp-valued string is the one
;; place a gexp enters the IR; the emitter splices it between the surrounding
;; quotes.

(define (gascity-records-maybe-string? value)
  "Return #t if VALUE is a string or `unset'."
  (or (eq? value 'unset) (string? value)))

(define (gascity-records-string-or-gexp? value)
  "Return #t if VALUE is a string, a gexp or `unset'."
  (or (eq? value 'unset) (string? value) (gexp? value)))

(define (gascity-records-serialize-maybe-string key value)
  "Return a field node for KEY with VALUE, or nothing when VALUE is `unset'."
  (if (eq? value 'unset)
      '()
      (list (toml-field key value))))

(define (gascity-records-serialize-string-or-gexp key value)
  "Return a field node for KEY with VALUE, a string or a gexp, or nothing
when VALUE is `unset'."
  (cond
   ((eq? value 'unset) '())
   ((or (string? value) (gexp? value)) (list (toml-field key value)))
   (else
    (error "gascity-records: field must be a string, a gexp or 'unset" key value))))

(define (gascity-records-serialize-tri-state key value)
  "Return a boolean field node for KEY, or nothing when VALUE is `unset'."
  (cond
   ((eq? value 'unset) '())
   ((boolean? value) (list (toml-field key value)))
   (else
    (error "gascity-records: tri-state field must be 'unset, #t or #f" key value))))

(define (gascity-records-serialize-maybe-integer key value)
  "Return an integer field node for KEY, or nothing when VALUE is `unset'."
  (cond
   ((eq? value 'unset) '())
   ((exact-integer? value) (list (toml-field key value)))
   (else
    (error "gascity-records: integer field must be an exact integer or 'unset" key value))))

(define (gascity-records-serialize-maybe-real key value)
  "Return a real field node for KEY, or nothing when VALUE is `unset'."
  (cond
   ((eq? value 'unset) '())
   ((real? value) (list (toml-field key value)))
   (else
    (error "gascity-records: real field must be a real number or 'unset" key value))))

(define (gascity-records-serialize-list-of-strings key value)
  "Return an array field node for KEY with the list VALUE, or nothing when
VALUE is `unset'."
  (if (eq? value 'unset)
      '()
      (list (toml-field key (apply toml-array value)))))

(define (gascity-records-serialize-list-of-lists-of-strings key value)
  "Return an array-of-arrays field node for KEY with the list VALUE, or
nothing when VALUE is `unset'."
  (if (eq? value 'unset)
      '()
      (list (toml-field key
                        (apply toml-array
                               (map (lambda (row) (apply toml-array row))
                                    value))))))

(define (gascity-records-serialize-string-map key value)
  "Return an inline-table field node for KEY with the alist VALUE, or nothing
when VALUE is `unset'."
  (if (eq? value 'unset)
      '()
      (list (toml-field key (apply toml-inline-table value)))))

(define (gascity-records-serialize-subtable key value proc)
  "Return a `[KEY]' table node whose body is (PROC VALUE), or nothing when
VALUE is `unset' or has an empty body."
  (if (eq? value 'unset)
      '()
      (let ((nodes (proc value)))
        (if (null? nodes) '() (list (apply toml-table key nodes))))))

(define (gascity-records-serialize-subtables key values proc)
  "Return a `[[KEY]]' array-of-tables node with one element per element of
VALUES, each element the body (PROC VALUE), or nothing when VALUES is `unset'
or empty."
  (if (or (eq? values 'unset) (null? values))
      '()
      (list (apply toml-array-of-tables key (map proc values)))))

(define (gascity-records-serialize-alist-subtables key value proc)
  "Return a `[KEY]' table node holding one `[KEY.NAME]' child table per
(NAME . ITEM) pair of the alist VALUE, each child the body (PROC ITEM), or
nothing when VALUE is `unset' or empty."
  (if (or (eq? value 'unset) (null? value))
      '()
      (list (apply toml-table key
                   (map (lambda (entry)
                          (apply toml-table (car entry) (proc (cdr entry))))
                        value)))))


;;;
;;; The literal-secret guard.
;;;

;; A conservative guard, shared with the hand-written service module: it
;; rejects the well-known credential prefixes and long opaque tokens, and
;; accepts `$VAR' references, ordinary paths, `sha:' pins and `builtin:' names.

(define %gascity-records-secret-prefixes
  (list "sk-ant-" "ghp_" "gho_" "ghu_" "ghs_" "ghr_" "github_pat_"
        "xoxb-" "xoxp-" "xoxa-" "xoxr-" "xoxs-" "glpat-" "AIza" "AKIA"))

(define (gascity-records-secret-character? character)
  "Return #t if CHARACTER may appear in an opaque token."
  (or (char-alphabetic? character)
      (char-numeric? character)
      (memv character '(#\_ #\+ #\- #\=))))

(define (gascity-records-high-entropy-token? string)
  "Return #t if STRING looks like an opaque credential: long, free of
path/URL/namespace punctuation, and mixing letters and digits."
  (and (> (string-length string) 40)
       (string-every gascity-records-secret-character? string)
       (string-any char-numeric? string)
       (string-any char-alphabetic? string)))

(define (gascity-records-literal-secret? value)
  "Return #t if VALUE, a string, is an obviously literal secret.  A
`NAME=value' assignment is judged on its value part; a value that is a `$VAR'
reference is not a secret."
  (and (string? value)
       (let* ((string (string-trim-both value))
              (string (if (string-prefix? "export " string)
                          (string-trim (substring string 7))
                          string))
              (equals (string-index string #\=))
              (string (if equals (substring string (+ equals 1)) string)))
         (and (not (string-null? string))
              (not (string-prefix? "$" string))
              (or (any (lambda (prefix) (string-contains string prefix))
                       %gascity-records-secret-prefixes)
                  (gascity-records-high-entropy-token? string))))))

(define (gascity-records-serialize-secret key value)
  "Return a field node for the secret field KEY with VALUE, or nothing when
VALUE is `unset'.  Raise when VALUE is an obviously literal secret; a `$VAR'
reference or an ordinary env-var name is emitted verbatim."
  (cond
   ((eq? value 'unset) '())
   ((gascity-records-literal-secret? value)
    (error "gascity-records: refusing the literal secret; write a $VAR reference" key value))
   (else (list (toml-field key value)))))


;;;
;;; Leaf configuration records (75 types).
;;;

;; ACPSessionConfig — from `$defs.ACPSessionConfig'.

(define-record-type* <gascity-acp-session-configuration>
  gascity-acp-session-configuration
  make-gascity-acp-session-configuration
  gascity-acp-session-configuration?
  (handshake-timeout
   gascity-acp-session-configuration-handshake-timeout
   (default 'unset))
  (nudge-busy-timeout
   gascity-acp-session-configuration-nudge-busy-timeout
   (default 'unset))
  (output-buffer-lines
   gascity-acp-session-configuration-output-buffer-lines
   (default 'unset))
  (stop-grace
   gascity-acp-session-configuration-stop-grace
   (default 'unset))
  )

(define (gascity-acp-session-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-acp-session-configuration>."
  (match-record config <gascity-acp-session-configuration>
    (handshake-timeout nudge-busy-timeout output-buffer-lines stop-grace)
    (append
     (gascity-records-serialize-maybe-string "handshake_timeout" handshake-timeout)
     (gascity-records-serialize-maybe-string "nudge_busy_timeout" nudge-busy-timeout)
     (gascity-records-serialize-maybe-integer "output_buffer_lines" output-buffer-lines)
     (gascity-records-serialize-maybe-string "stop_grace" stop-grace)
     '())))

;; APIConfig — from `$defs.APIConfig'.

(define-record-type* <gascity-api-configuration>
  gascity-api-configuration
  make-gascity-api-configuration
  gascity-api-configuration?
  (allow-mutations
   gascity-api-configuration-allow-mutations
   (default 'unset))
  (bind
   gascity-api-configuration-bind
   (default 'unset))
  (port
   gascity-api-configuration-port
   (default 'unset))
  (read-auth-required
   gascity-api-configuration-read-auth-required
   (default 'unset))
  (read-auth-verify-key
   gascity-api-configuration-read-auth-verify-key
   (default 'unset))
  (write-auth-allow-unverified
   gascity-api-configuration-write-auth-allow-unverified
   (default 'unset))
  (write-auth-required
   gascity-api-configuration-write-auth-required
   (default 'unset))
  (write-auth-verify-key
   gascity-api-configuration-write-auth-verify-key
   (default 'unset))
  )

(define (gascity-api-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-api-configuration>."
  (match-record config <gascity-api-configuration>
    (allow-mutations bind port read-auth-required read-auth-verify-key write-auth-allow-unverified write-auth-required write-auth-verify-key)
    (append
     (gascity-records-serialize-tri-state "allow_mutations" allow-mutations)
     (gascity-records-serialize-maybe-string "bind" bind)
     (gascity-records-serialize-maybe-integer "port" port)
     (gascity-records-serialize-tri-state "read_auth_required" read-auth-required)
     (gascity-records-serialize-maybe-string "read_auth_verify_key" read-auth-verify-key)
     (gascity-records-serialize-tri-state "write_auth_allow_unverified" write-auth-allow-unverified)
     (gascity-records-serialize-tri-state "write_auth_required" write-auth-required)
     (gascity-records-serialize-maybe-string "write_auth_verify_key" write-auth-verify-key)
     '())))

;; Agent — from `$defs.Agent'.

(define-record-type* <gascity-agent-configuration>
  gascity-agent-configuration
  make-gascity-agent-configuration
  gascity-agent-configuration?
  (append-fragments
   gascity-agent-configuration-append-fragments
   (default 'unset))
  (args
   gascity-agent-configuration-args
   (default 'unset))
  (assigned-work-defer-limit
   gascity-agent-configuration-assigned-work-defer-limit
   (default 'unset))
  (attach
   gascity-agent-configuration-attach
   (default 'unset))
  (auto-reclaim-stale-claims
   gascity-agent-configuration-auto-reclaim-stale-claims
   (default 'unset))
  (context-advisory
   gascity-agent-configuration-context-advisory
   (default 'unset))
  (default-sling-formula
   gascity-agent-configuration-default-sling-formula
   (default 'unset))
  (depends-on
   gascity-agent-configuration-depends-on
   (default 'unset))
  (description
   gascity-agent-configuration-description
   (default 'unset))
  (dir
   gascity-agent-configuration-dir
   (default 'unset))
  (drain-timeout
   gascity-agent-configuration-drain-timeout
   (default 'unset))
  (emits-permission-warning
   gascity-agent-configuration-emits-permission-warning
   (default 'unset))
  (env
   gascity-agent-configuration-env
   (default 'unset))
  (hooks-installed
   gascity-agent-configuration-hooks-installed
   (default 'unset))
  (idle-timeout
   gascity-agent-configuration-idle-timeout
   (default 'unset))
  (inject-assigned-skills
   gascity-agent-configuration-inject-assigned-skills
   (default 'unset))
  (inject-fragments
   gascity-agent-configuration-inject-fragments
   (default 'unset))
  (install-agent-hooks
   gascity-agent-configuration-install-agent-hooks
   (default 'unset))
  (lifecycle
   gascity-agent-configuration-lifecycle
   (default 'unset))
  (max-active-sessions
   gascity-agent-configuration-max-active-sessions
   (default 'unset))
  (max-session-age
   gascity-agent-configuration-max-session-age
   (default 'unset))
  (max-session-age-jitter
   gascity-agent-configuration-max-session-age-jitter
   (default 'unset))
  (mcp
   gascity-agent-configuration-mcp
   (default 'unset))
  (min-active-sessions
   gascity-agent-configuration-min-active-sessions
   (default 'unset))
  (mouse-mode
   gascity-agent-configuration-mouse-mode
   (default 'unset))
  (name
   gascity-agent-configuration-name
   (default 'unset))
  (namepool
   gascity-agent-configuration-namepool
   (default 'unset))
  (nudge
   gascity-agent-configuration-nudge
   (default 'unset))
  (on-boot
   gascity-agent-configuration-on-boot
   (default 'unset))
  (on-death
   gascity-agent-configuration-on-death
   (default 'unset))
  (option-defaults
   gascity-agent-configuration-option-defaults
   (default 'unset))
  (overlay-dir
   gascity-agent-configuration-overlay-dir
   (default 'unset))
  (pre-start
   gascity-agent-configuration-pre-start
   (default 'unset))
  (process-names
   gascity-agent-configuration-process-names
   (default 'unset))
  (prompt-flag
   gascity-agent-configuration-prompt-flag
   (default 'unset))
  (prompt-mode
   gascity-agent-configuration-prompt-mode
   (default 'unset))
  (prompt-template
   gascity-agent-configuration-prompt-template
   (default 'unset))
  (provider
   gascity-agent-configuration-provider
   (default 'unset))
  (ready-delay-ms
   gascity-agent-configuration-ready-delay-ms
   (default 'unset))
  (ready-prompt-prefix
   gascity-agent-configuration-ready-prompt-prefix
   (default 'unset))
  (resume-command
   gascity-agent-configuration-resume-command
   (default 'unset))
  (scale-check
   gascity-agent-configuration-scale-check
   (default 'unset))
  (scope
   gascity-agent-configuration-scope
   (default 'unset))
  (session
   gascity-agent-configuration-session
   (default 'unset))
  (session-live
   gascity-agent-configuration-session-live
   (default 'unset))
  (session-setup
   gascity-agent-configuration-session-setup
   (default 'unset))
  (session-setup-script
   gascity-agent-configuration-session-setup-script
   (default 'unset))
  (skills
   gascity-agent-configuration-skills
   (default 'unset))
  (sleep-after-idle
   gascity-agent-configuration-sleep-after-idle
   (default 'unset))
  (sling-query
   gascity-agent-configuration-sling-query
   (default 'unset))
  (start-command
   gascity-agent-configuration-start-command
   (default 'unset))
  (suspended
   gascity-agent-configuration-suspended
   (default 'unset))
  (tmux-alias
   gascity-agent-configuration-tmux-alias
   (default 'unset))
  (upstream
   gascity-agent-configuration-upstream
   (default 'unset))
  (wake-mode
   gascity-agent-configuration-wake-mode
   (default 'unset))
  (work-dir
   gascity-agent-configuration-work-dir
   (default 'unset))
  (work-query
   gascity-agent-configuration-work-query
   (default 'unset))
  )

(define (gascity-agent-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-agent-configuration>."
  (match-record config <gascity-agent-configuration>
    (append-fragments args assigned-work-defer-limit attach auto-reclaim-stale-claims context-advisory default-sling-formula depends-on description dir drain-timeout emits-permission-warning env hooks-installed idle-timeout inject-assigned-skills inject-fragments install-agent-hooks lifecycle max-active-sessions max-session-age max-session-age-jitter mcp min-active-sessions mouse-mode name namepool nudge on-boot on-death option-defaults overlay-dir pre-start process-names prompt-flag prompt-mode prompt-template provider ready-delay-ms ready-prompt-prefix resume-command scale-check scope session session-live session-setup session-setup-script skills sleep-after-idle sling-query start-command suspended tmux-alias upstream wake-mode work-dir work-query)
    (append
     (gascity-records-serialize-list-of-strings "append_fragments" append-fragments)
     (gascity-records-serialize-list-of-strings "args" args)
     (gascity-records-serialize-maybe-integer "assigned_work_defer_limit" assigned-work-defer-limit)
     (gascity-records-serialize-tri-state "attach" attach)
     (gascity-records-serialize-tri-state "auto_reclaim_stale_claims" auto-reclaim-stale-claims)
     (gascity-records-serialize-subtable "context_advisory" context-advisory gascity-context-advisory-configuration->toml)
     (gascity-records-serialize-maybe-string "default_sling_formula" default-sling-formula)
     (gascity-records-serialize-list-of-strings "depends_on" depends-on)
     (gascity-records-serialize-maybe-string "description" description)
     (gascity-records-serialize-maybe-string "dir" dir)
     (gascity-records-serialize-maybe-string "drain_timeout" drain-timeout)
     (gascity-records-serialize-tri-state "emits_permission_warning" emits-permission-warning)
     (gascity-records-serialize-string-map "env" env)
     (gascity-records-serialize-tri-state "hooks_installed" hooks-installed)
     (gascity-records-serialize-maybe-string "idle_timeout" idle-timeout)
     (gascity-records-serialize-tri-state "inject_assigned_skills" inject-assigned-skills)
     (gascity-records-serialize-list-of-strings "inject_fragments" inject-fragments)
     (gascity-records-serialize-list-of-strings "install_agent_hooks" install-agent-hooks)
     (gascity-records-serialize-maybe-string "lifecycle" lifecycle)
     (gascity-records-serialize-maybe-integer "max_active_sessions" max-active-sessions)
     (gascity-records-serialize-maybe-string "max_session_age" max-session-age)
     (gascity-records-serialize-maybe-string "max_session_age_jitter" max-session-age-jitter)
     (gascity-records-serialize-list-of-strings "mcp" mcp)
     (gascity-records-serialize-maybe-integer "min_active_sessions" min-active-sessions)
     (gascity-records-serialize-maybe-string "mouse_mode" mouse-mode)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-maybe-string "namepool" namepool)
     (gascity-records-serialize-maybe-string "nudge" nudge)
     (gascity-records-serialize-string-or-gexp "on_boot" on-boot)
     (gascity-records-serialize-string-or-gexp "on_death" on-death)
     (gascity-records-serialize-string-map "option_defaults" option-defaults)
     (gascity-records-serialize-maybe-string "overlay_dir" overlay-dir)
     (gascity-records-serialize-list-of-strings "pre_start" pre-start)
     (gascity-records-serialize-list-of-strings "process_names" process-names)
     (gascity-records-serialize-maybe-string "prompt_flag" prompt-flag)
     (gascity-records-serialize-maybe-string "prompt_mode" prompt-mode)
     (gascity-records-serialize-maybe-string "prompt_template" prompt-template)
     (gascity-records-serialize-maybe-string "provider" provider)
     (gascity-records-serialize-maybe-integer "ready_delay_ms" ready-delay-ms)
     (gascity-records-serialize-maybe-string "ready_prompt_prefix" ready-prompt-prefix)
     (gascity-records-serialize-string-or-gexp "resume_command" resume-command)
     (gascity-records-serialize-string-or-gexp "scale_check" scale-check)
     (gascity-records-serialize-maybe-string "scope" scope)
     (gascity-records-serialize-maybe-string "session" session)
     (gascity-records-serialize-list-of-strings "session_live" session-live)
     (gascity-records-serialize-list-of-strings "session_setup" session-setup)
     (gascity-records-serialize-string-or-gexp "session_setup_script" session-setup-script)
     (gascity-records-serialize-list-of-strings "skills" skills)
     (gascity-records-serialize-maybe-string "sleep_after_idle" sleep-after-idle)
     (gascity-records-serialize-string-or-gexp "sling_query" sling-query)
     (gascity-records-serialize-string-or-gexp "start_command" start-command)
     (gascity-records-serialize-tri-state "suspended" suspended)
     (gascity-records-serialize-maybe-string "tmux_alias" tmux-alias)
     (gascity-records-serialize-maybe-string "upstream" upstream)
     (gascity-records-serialize-maybe-string "wake_mode" wake-mode)
     (gascity-records-serialize-maybe-string "work_dir" work-dir)
     (gascity-records-serialize-string-or-gexp "work_query" work-query)
     '())))

;; AgentDefaults — from `$defs.AgentDefaults'.

(define-record-type* <gascity-agent-defaults-configuration>
  gascity-agent-defaults-configuration
  make-gascity-agent-defaults-configuration
  gascity-agent-defaults-configuration?
  (allow-env-override
   gascity-agent-defaults-configuration-allow-env-override
   (default 'unset))
  (allow-overlay
   gascity-agent-defaults-configuration-allow-overlay
   (default 'unset))
  (append-fragments
   gascity-agent-defaults-configuration-append-fragments
   (default 'unset))
  (context-advisory
   gascity-agent-defaults-configuration-context-advisory
   (default 'unset))
  (default-sling-formula
   gascity-agent-defaults-configuration-default-sling-formula
   (default 'unset))
  (mcp
   gascity-agent-defaults-configuration-mcp
   (default 'unset))
  (model
   gascity-agent-defaults-configuration-model
   (default 'unset))
  (provider
   gascity-agent-defaults-configuration-provider
   (default 'unset))
  (skills
   gascity-agent-defaults-configuration-skills
   (default 'unset))
  (upstream
   gascity-agent-defaults-configuration-upstream
   (default 'unset))
  (wake-mode
   gascity-agent-defaults-configuration-wake-mode
   (default 'unset))
  )

(define (gascity-agent-defaults-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-agent-defaults-configuration>."
  (match-record config <gascity-agent-defaults-configuration>
    (allow-env-override allow-overlay append-fragments context-advisory default-sling-formula mcp model provider skills upstream wake-mode)
    (append
     (gascity-records-serialize-list-of-strings "allow_env_override" allow-env-override)
     (gascity-records-serialize-list-of-strings "allow_overlay" allow-overlay)
     (gascity-records-serialize-list-of-strings "append_fragments" append-fragments)
     (gascity-records-serialize-subtable "context_advisory" context-advisory gascity-context-advisory-configuration->toml)
     (gascity-records-serialize-maybe-string "default_sling_formula" default-sling-formula)
     (gascity-records-serialize-list-of-strings "mcp" mcp)
     (gascity-records-serialize-maybe-string "model" model)
     (gascity-records-serialize-maybe-string "provider" provider)
     (gascity-records-serialize-list-of-strings "skills" skills)
     (gascity-records-serialize-maybe-string "upstream" upstream)
     (gascity-records-serialize-maybe-string "wake_mode" wake-mode)
     '())))

;; AgentOverride — from `$defs.AgentOverride'.

(define-record-type* <gascity-agent-override-configuration>
  gascity-agent-override-configuration
  make-gascity-agent-override-configuration
  gascity-agent-override-configuration?
  (agent
   gascity-agent-override-configuration-agent
   (default 'unset))
  (append-fragments
   gascity-agent-override-configuration-append-fragments
   (default 'unset))
  (args
   gascity-agent-override-configuration-args
   (default 'unset))
  (assigned-work-defer-limit
   gascity-agent-override-configuration-assigned-work-defer-limit
   (default 'unset))
  (attach
   gascity-agent-override-configuration-attach
   (default 'unset))
  (auto-reclaim-stale-claims
   gascity-agent-override-configuration-auto-reclaim-stale-claims
   (default 'unset))
  (context-advisory
   gascity-agent-override-configuration-context-advisory
   (default 'unset))
  (default-sling-formula
   gascity-agent-override-configuration-default-sling-formula
   (default 'unset))
  (depends-on
   gascity-agent-override-configuration-depends-on
   (default 'unset))
  (dir
   gascity-agent-override-configuration-dir
   (default 'unset))
  (env
   gascity-agent-override-configuration-env
   (default 'unset))
  (env-remove
   gascity-agent-override-configuration-env-remove
   (default 'unset))
  (hooks-installed
   gascity-agent-override-configuration-hooks-installed
   (default 'unset))
  (idle-timeout
   gascity-agent-override-configuration-idle-timeout
   (default 'unset))
  (inject-assigned-skills
   gascity-agent-override-configuration-inject-assigned-skills
   (default 'unset))
  (inject-fragments
   gascity-agent-override-configuration-inject-fragments
   (default 'unset))
  (inject-fragments-append
   gascity-agent-override-configuration-inject-fragments-append
   (default 'unset))
  (install-agent-hooks
   gascity-agent-override-configuration-install-agent-hooks
   (default 'unset))
  (install-agent-hooks-append
   gascity-agent-override-configuration-install-agent-hooks-append
   (default 'unset))
  (lifecycle
   gascity-agent-override-configuration-lifecycle
   (default 'unset))
  (max-active-sessions
   gascity-agent-override-configuration-max-active-sessions
   (default 'unset))
  (max-session-age
   gascity-agent-override-configuration-max-session-age
   (default 'unset))
  (max-session-age-jitter
   gascity-agent-override-configuration-max-session-age-jitter
   (default 'unset))
  (mcp
   gascity-agent-override-configuration-mcp
   (default 'unset))
  (mcp-append
   gascity-agent-override-configuration-mcp-append
   (default 'unset))
  (min-active-sessions
   gascity-agent-override-configuration-min-active-sessions
   (default 'unset))
  (mouse-mode
   gascity-agent-override-configuration-mouse-mode
   (default 'unset))
  (nudge
   gascity-agent-override-configuration-nudge
   (default 'unset))
  (option-defaults
   gascity-agent-override-configuration-option-defaults
   (default 'unset))
  (overlay-dir
   gascity-agent-override-configuration-overlay-dir
   (default 'unset))
  (pool
   gascity-agent-override-configuration-pool
   (default 'unset))
  (pre-start
   gascity-agent-override-configuration-pre-start
   (default 'unset))
  (pre-start-append
   gascity-agent-override-configuration-pre-start-append
   (default 'unset))
  (prompt-template
   gascity-agent-override-configuration-prompt-template
   (default 'unset))
  (provider
   gascity-agent-override-configuration-provider
   (default 'unset))
  (resume-command
   gascity-agent-override-configuration-resume-command
   (default 'unset))
  (scale-check
   gascity-agent-override-configuration-scale-check
   (default 'unset))
  (scope
   gascity-agent-override-configuration-scope
   (default 'unset))
  (session
   gascity-agent-override-configuration-session
   (default 'unset))
  (session-live
   gascity-agent-override-configuration-session-live
   (default 'unset))
  (session-live-append
   gascity-agent-override-configuration-session-live-append
   (default 'unset))
  (session-setup
   gascity-agent-override-configuration-session-setup
   (default 'unset))
  (session-setup-append
   gascity-agent-override-configuration-session-setup-append
   (default 'unset))
  (session-setup-script
   gascity-agent-override-configuration-session-setup-script
   (default 'unset))
  (skills
   gascity-agent-override-configuration-skills
   (default 'unset))
  (skills-append
   gascity-agent-override-configuration-skills-append
   (default 'unset))
  (sleep-after-idle
   gascity-agent-override-configuration-sleep-after-idle
   (default 'unset))
  (start-command
   gascity-agent-override-configuration-start-command
   (default 'unset))
  (suspended
   gascity-agent-override-configuration-suspended
   (default 'unset))
  (tmux-alias
   gascity-agent-override-configuration-tmux-alias
   (default 'unset))
  (upstream
   gascity-agent-override-configuration-upstream
   (default 'unset))
  (wake-mode
   gascity-agent-override-configuration-wake-mode
   (default 'unset))
  (work-dir
   gascity-agent-override-configuration-work-dir
   (default 'unset))
  )

(define (gascity-agent-override-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-agent-override-configuration>."
  (match-record config <gascity-agent-override-configuration>
    (agent append-fragments args assigned-work-defer-limit attach auto-reclaim-stale-claims context-advisory default-sling-formula depends-on dir env env-remove hooks-installed idle-timeout inject-assigned-skills inject-fragments inject-fragments-append install-agent-hooks install-agent-hooks-append lifecycle max-active-sessions max-session-age max-session-age-jitter mcp mcp-append min-active-sessions mouse-mode nudge option-defaults overlay-dir pool pre-start pre-start-append prompt-template provider resume-command scale-check scope session session-live session-live-append session-setup session-setup-append session-setup-script skills skills-append sleep-after-idle start-command suspended tmux-alias upstream wake-mode work-dir)
    (append
     (gascity-records-serialize-maybe-string "agent" agent)
     (gascity-records-serialize-list-of-strings "append_fragments" append-fragments)
     (gascity-records-serialize-list-of-strings "args" args)
     (gascity-records-serialize-maybe-integer "assigned_work_defer_limit" assigned-work-defer-limit)
     (gascity-records-serialize-tri-state "attach" attach)
     (gascity-records-serialize-tri-state "auto_reclaim_stale_claims" auto-reclaim-stale-claims)
     (gascity-records-serialize-subtable "context_advisory" context-advisory gascity-context-advisory-configuration->toml)
     (gascity-records-serialize-maybe-string "default_sling_formula" default-sling-formula)
     (gascity-records-serialize-list-of-strings "depends_on" depends-on)
     (gascity-records-serialize-maybe-string "dir" dir)
     (gascity-records-serialize-string-map "env" env)
     (gascity-records-serialize-list-of-strings "env_remove" env-remove)
     (gascity-records-serialize-tri-state "hooks_installed" hooks-installed)
     (gascity-records-serialize-maybe-string "idle_timeout" idle-timeout)
     (gascity-records-serialize-tri-state "inject_assigned_skills" inject-assigned-skills)
     (gascity-records-serialize-list-of-strings "inject_fragments" inject-fragments)
     (gascity-records-serialize-list-of-strings "inject_fragments_append" inject-fragments-append)
     (gascity-records-serialize-list-of-strings "install_agent_hooks" install-agent-hooks)
     (gascity-records-serialize-list-of-strings "install_agent_hooks_append" install-agent-hooks-append)
     (gascity-records-serialize-maybe-string "lifecycle" lifecycle)
     (gascity-records-serialize-maybe-integer "max_active_sessions" max-active-sessions)
     (gascity-records-serialize-maybe-string "max_session_age" max-session-age)
     (gascity-records-serialize-maybe-string "max_session_age_jitter" max-session-age-jitter)
     (gascity-records-serialize-list-of-strings "mcp" mcp)
     (gascity-records-serialize-list-of-strings "mcp_append" mcp-append)
     (gascity-records-serialize-maybe-integer "min_active_sessions" min-active-sessions)
     (gascity-records-serialize-maybe-string "mouse_mode" mouse-mode)
     (gascity-records-serialize-maybe-string "nudge" nudge)
     (gascity-records-serialize-string-map "option_defaults" option-defaults)
     (gascity-records-serialize-maybe-string "overlay_dir" overlay-dir)
     (gascity-records-serialize-subtable "pool" pool gascity-pool-override-configuration->toml)
     (gascity-records-serialize-list-of-strings "pre_start" pre-start)
     (gascity-records-serialize-list-of-strings "pre_start_append" pre-start-append)
     (gascity-records-serialize-maybe-string "prompt_template" prompt-template)
     (gascity-records-serialize-maybe-string "provider" provider)
     (gascity-records-serialize-string-or-gexp "resume_command" resume-command)
     (gascity-records-serialize-string-or-gexp "scale_check" scale-check)
     (gascity-records-serialize-maybe-string "scope" scope)
     (gascity-records-serialize-maybe-string "session" session)
     (gascity-records-serialize-list-of-strings "session_live" session-live)
     (gascity-records-serialize-list-of-strings "session_live_append" session-live-append)
     (gascity-records-serialize-list-of-strings "session_setup" session-setup)
     (gascity-records-serialize-list-of-strings "session_setup_append" session-setup-append)
     (gascity-records-serialize-string-or-gexp "session_setup_script" session-setup-script)
     (gascity-records-serialize-list-of-strings "skills" skills)
     (gascity-records-serialize-list-of-strings "skills_append" skills-append)
     (gascity-records-serialize-maybe-string "sleep_after_idle" sleep-after-idle)
     (gascity-records-serialize-string-or-gexp "start_command" start-command)
     (gascity-records-serialize-tri-state "suspended" suspended)
     (gascity-records-serialize-maybe-string "tmux_alias" tmux-alias)
     (gascity-records-serialize-maybe-string "upstream" upstream)
     (gascity-records-serialize-maybe-string "wake_mode" wake-mode)
     (gascity-records-serialize-maybe-string "work_dir" work-dir)
     '())))

;; AgentPatch — from `$defs.AgentPatch'.

(define-record-type* <gascity-agent-patch-configuration>
  gascity-agent-patch-configuration
  make-gascity-agent-patch-configuration
  gascity-agent-patch-configuration?
  (append-fragments
   gascity-agent-patch-configuration-append-fragments
   (default 'unset))
  (args
   gascity-agent-patch-configuration-args
   (default 'unset))
  (assigned-work-defer-limit
   gascity-agent-patch-configuration-assigned-work-defer-limit
   (default 'unset))
  (attach
   gascity-agent-patch-configuration-attach
   (default 'unset))
  (auto-reclaim-stale-claims
   gascity-agent-patch-configuration-auto-reclaim-stale-claims
   (default 'unset))
  (context-advisory
   gascity-agent-patch-configuration-context-advisory
   (default 'unset))
  (default-sling-formula
   gascity-agent-patch-configuration-default-sling-formula
   (default 'unset))
  (depends-on
   gascity-agent-patch-configuration-depends-on
   (default 'unset))
  (dir
   gascity-agent-patch-configuration-dir
   (default 'unset))
  (env
   gascity-agent-patch-configuration-env
   (default 'unset))
  (env-remove
   gascity-agent-patch-configuration-env-remove
   (default 'unset))
  (hooks-installed
   gascity-agent-patch-configuration-hooks-installed
   (default 'unset))
  (idle-timeout
   gascity-agent-patch-configuration-idle-timeout
   (default 'unset))
  (inject-assigned-skills
   gascity-agent-patch-configuration-inject-assigned-skills
   (default 'unset))
  (inject-fragments
   gascity-agent-patch-configuration-inject-fragments
   (default 'unset))
  (inject-fragments-append
   gascity-agent-patch-configuration-inject-fragments-append
   (default 'unset))
  (install-agent-hooks
   gascity-agent-patch-configuration-install-agent-hooks
   (default 'unset))
  (install-agent-hooks-append
   gascity-agent-patch-configuration-install-agent-hooks-append
   (default 'unset))
  (lifecycle
   gascity-agent-patch-configuration-lifecycle
   (default 'unset))
  (max-active-sessions
   gascity-agent-patch-configuration-max-active-sessions
   (default 'unset))
  (max-session-age
   gascity-agent-patch-configuration-max-session-age
   (default 'unset))
  (max-session-age-jitter
   gascity-agent-patch-configuration-max-session-age-jitter
   (default 'unset))
  (mcp
   gascity-agent-patch-configuration-mcp
   (default 'unset))
  (mcp-append
   gascity-agent-patch-configuration-mcp-append
   (default 'unset))
  (min-active-sessions
   gascity-agent-patch-configuration-min-active-sessions
   (default 'unset))
  (mouse-mode
   gascity-agent-patch-configuration-mouse-mode
   (default 'unset))
  (name
   gascity-agent-patch-configuration-name
   (default 'unset))
  (nudge
   gascity-agent-patch-configuration-nudge
   (default 'unset))
  (option-defaults
   gascity-agent-patch-configuration-option-defaults
   (default 'unset))
  (overlay-dir
   gascity-agent-patch-configuration-overlay-dir
   (default 'unset))
  (pool
   gascity-agent-patch-configuration-pool
   (default 'unset))
  (pre-start
   gascity-agent-patch-configuration-pre-start
   (default 'unset))
  (pre-start-append
   gascity-agent-patch-configuration-pre-start-append
   (default 'unset))
  (prompt-template
   gascity-agent-patch-configuration-prompt-template
   (default 'unset))
  (provider
   gascity-agent-patch-configuration-provider
   (default 'unset))
  (resume-command
   gascity-agent-patch-configuration-resume-command
   (default 'unset))
  (rig
   gascity-agent-patch-configuration-rig
   (default 'unset))
  (scale-check
   gascity-agent-patch-configuration-scale-check
   (default 'unset))
  (scope
   gascity-agent-patch-configuration-scope
   (default 'unset))
  (session
   gascity-agent-patch-configuration-session
   (default 'unset))
  (session-live
   gascity-agent-patch-configuration-session-live
   (default 'unset))
  (session-live-append
   gascity-agent-patch-configuration-session-live-append
   (default 'unset))
  (session-setup
   gascity-agent-patch-configuration-session-setup
   (default 'unset))
  (session-setup-append
   gascity-agent-patch-configuration-session-setup-append
   (default 'unset))
  (session-setup-script
   gascity-agent-patch-configuration-session-setup-script
   (default 'unset))
  (skills
   gascity-agent-patch-configuration-skills
   (default 'unset))
  (skills-append
   gascity-agent-patch-configuration-skills-append
   (default 'unset))
  (sleep-after-idle
   gascity-agent-patch-configuration-sleep-after-idle
   (default 'unset))
  (start-command
   gascity-agent-patch-configuration-start-command
   (default 'unset))
  (suspended
   gascity-agent-patch-configuration-suspended
   (default 'unset))
  (tmux-alias
   gascity-agent-patch-configuration-tmux-alias
   (default 'unset))
  (upstream
   gascity-agent-patch-configuration-upstream
   (default 'unset))
  (wake-mode
   gascity-agent-patch-configuration-wake-mode
   (default 'unset))
  (work-dir
   gascity-agent-patch-configuration-work-dir
   (default 'unset))
  )

(define (gascity-agent-patch-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-agent-patch-configuration>."
  (match-record config <gascity-agent-patch-configuration>
    (append-fragments args assigned-work-defer-limit attach auto-reclaim-stale-claims context-advisory default-sling-formula depends-on dir env env-remove hooks-installed idle-timeout inject-assigned-skills inject-fragments inject-fragments-append install-agent-hooks install-agent-hooks-append lifecycle max-active-sessions max-session-age max-session-age-jitter mcp mcp-append min-active-sessions mouse-mode name nudge option-defaults overlay-dir pool pre-start pre-start-append prompt-template provider resume-command rig scale-check scope session session-live session-live-append session-setup session-setup-append session-setup-script skills skills-append sleep-after-idle start-command suspended tmux-alias upstream wake-mode work-dir)
    (append
     (gascity-records-serialize-list-of-strings "append_fragments" append-fragments)
     (gascity-records-serialize-list-of-strings "args" args)
     (gascity-records-serialize-maybe-integer "assigned_work_defer_limit" assigned-work-defer-limit)
     (gascity-records-serialize-tri-state "attach" attach)
     (gascity-records-serialize-tri-state "auto_reclaim_stale_claims" auto-reclaim-stale-claims)
     (gascity-records-serialize-subtable "context_advisory" context-advisory gascity-context-advisory-configuration->toml)
     (gascity-records-serialize-maybe-string "default_sling_formula" default-sling-formula)
     (gascity-records-serialize-list-of-strings "depends_on" depends-on)
     (gascity-records-serialize-maybe-string "dir" dir)
     (gascity-records-serialize-string-map "env" env)
     (gascity-records-serialize-list-of-strings "env_remove" env-remove)
     (gascity-records-serialize-tri-state "hooks_installed" hooks-installed)
     (gascity-records-serialize-maybe-string "idle_timeout" idle-timeout)
     (gascity-records-serialize-tri-state "inject_assigned_skills" inject-assigned-skills)
     (gascity-records-serialize-list-of-strings "inject_fragments" inject-fragments)
     (gascity-records-serialize-list-of-strings "inject_fragments_append" inject-fragments-append)
     (gascity-records-serialize-list-of-strings "install_agent_hooks" install-agent-hooks)
     (gascity-records-serialize-list-of-strings "install_agent_hooks_append" install-agent-hooks-append)
     (gascity-records-serialize-maybe-string "lifecycle" lifecycle)
     (gascity-records-serialize-maybe-integer "max_active_sessions" max-active-sessions)
     (gascity-records-serialize-maybe-string "max_session_age" max-session-age)
     (gascity-records-serialize-maybe-string "max_session_age_jitter" max-session-age-jitter)
     (gascity-records-serialize-list-of-strings "mcp" mcp)
     (gascity-records-serialize-list-of-strings "mcp_append" mcp-append)
     (gascity-records-serialize-maybe-integer "min_active_sessions" min-active-sessions)
     (gascity-records-serialize-maybe-string "mouse_mode" mouse-mode)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-maybe-string "nudge" nudge)
     (gascity-records-serialize-string-map "option_defaults" option-defaults)
     (gascity-records-serialize-maybe-string "overlay_dir" overlay-dir)
     (gascity-records-serialize-subtable "pool" pool gascity-pool-override-configuration->toml)
     (gascity-records-serialize-list-of-strings "pre_start" pre-start)
     (gascity-records-serialize-list-of-strings "pre_start_append" pre-start-append)
     (gascity-records-serialize-maybe-string "prompt_template" prompt-template)
     (gascity-records-serialize-maybe-string "provider" provider)
     (gascity-records-serialize-string-or-gexp "resume_command" resume-command)
     (gascity-records-serialize-maybe-string "rig" rig)
     (gascity-records-serialize-string-or-gexp "scale_check" scale-check)
     (gascity-records-serialize-maybe-string "scope" scope)
     (gascity-records-serialize-maybe-string "session" session)
     (gascity-records-serialize-list-of-strings "session_live" session-live)
     (gascity-records-serialize-list-of-strings "session_live_append" session-live-append)
     (gascity-records-serialize-list-of-strings "session_setup" session-setup)
     (gascity-records-serialize-list-of-strings "session_setup_append" session-setup-append)
     (gascity-records-serialize-string-or-gexp "session_setup_script" session-setup-script)
     (gascity-records-serialize-list-of-strings "skills" skills)
     (gascity-records-serialize-list-of-strings "skills_append" skills-append)
     (gascity-records-serialize-maybe-string "sleep_after_idle" sleep-after-idle)
     (gascity-records-serialize-string-or-gexp "start_command" start-command)
     (gascity-records-serialize-tri-state "suspended" suspended)
     (gascity-records-serialize-maybe-string "tmux_alias" tmux-alias)
     (gascity-records-serialize-maybe-string "upstream" upstream)
     (gascity-records-serialize-maybe-string "wake_mode" wake-mode)
     (gascity-records-serialize-maybe-string "work_dir" work-dir)
     '())))

;; BeadPolicyConfig — from `$defs.BeadPolicyConfig'.

(define-record-type* <gascity-bead-policy-configuration>
  gascity-bead-policy-configuration
  make-gascity-bead-policy-configuration
  gascity-bead-policy-configuration?
  (delete-after-close
   gascity-bead-policy-configuration-delete-after-close
   (default 'unset))
  (storage
   gascity-bead-policy-configuration-storage
   (default 'unset))
  )

(define (gascity-bead-policy-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-bead-policy-configuration>."
  (match-record config <gascity-bead-policy-configuration>
    (delete-after-close storage)
    (append
     (gascity-records-serialize-maybe-string "delete_after_close" delete-after-close)
     (gascity-records-serialize-maybe-string "storage" storage)
     '())))

;; BeadsConfig — from `$defs.BeadsConfig'.

(define-record-type* <gascity-beads-configuration>
  gascity-beads-configuration
  make-gascity-beads-configuration
  gascity-beads-configuration?
  (backend
   gascity-beads-configuration-backend
   (default 'unset))
  (bd-compatibility
   gascity-beads-configuration-bd-compatibility
   (default 'unset))
  (conditional-writes
   gascity-beads-configuration-conditional-writes
   (default 'unset))
  (event-hooks
   gascity-beads-configuration-event-hooks
   (default 'unset))
  (guarded-release
   gascity-beads-configuration-guarded-release
   (default 'unset))
  (policies
   gascity-beads-configuration-policies
   (default 'unset))
  (provider
   gascity-beads-configuration-provider
   (default 'unset))
  )

(define (gascity-beads-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-beads-configuration>."
  (match-record config <gascity-beads-configuration>
    (backend bd-compatibility conditional-writes event-hooks guarded-release policies provider)
    (append
     (gascity-records-serialize-maybe-string "backend" backend)
     (gascity-records-serialize-maybe-string "bd_compatibility" bd-compatibility)
     (gascity-records-serialize-maybe-string "conditional_writes" conditional-writes)
     (gascity-records-serialize-tri-state "event_hooks" event-hooks)
     (gascity-records-serialize-maybe-string "guarded_release" guarded-release)
     (gascity-records-serialize-alist-subtables "policies" policies gascity-bead-policy-configuration->toml)
     (gascity-records-serialize-maybe-string "provider" provider)
     '())))

;; ChatSessionsConfig — from `$defs.ChatSessionsConfig'.

(define-record-type* <gascity-chat-sessions-configuration>
  gascity-chat-sessions-configuration
  make-gascity-chat-sessions-configuration
  gascity-chat-sessions-configuration?
  (grace-period
   gascity-chat-sessions-configuration-grace-period
   (default 'unset))
  (idle-timeout
   gascity-chat-sessions-configuration-idle-timeout
   (default 'unset))
  )

(define (gascity-chat-sessions-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-chat-sessions-configuration>."
  (match-record config <gascity-chat-sessions-configuration>
    (grace-period idle-timeout)
    (append
     (gascity-records-serialize-maybe-string "grace_period" grace-period)
     (gascity-records-serialize-maybe-string "idle_timeout" idle-timeout)
     '())))

;; City — from `$defs.City'.

(define-record-type* <gascity-city-configuration>
  gascity-city-configuration
  make-gascity-city-configuration
  gascity-city-configuration?
  (agent
   gascity-city-configuration-agent
   (default 'unset))
  (agent-defaults
   gascity-city-configuration-agent-defaults
   (default 'unset))
  (api
   gascity-city-configuration-api
   (default 'unset))
  (beads
   gascity-city-configuration-beads
   (default 'unset))
  (chat-sessions
   gascity-city-configuration-chat-sessions
   (default 'unset))
  (convergence
   gascity-city-configuration-convergence
   (default 'unset))
  (daemon
   gascity-city-configuration-daemon
   (default 'unset))
  (defaults
   gascity-city-configuration-defaults
   (default 'unset))
  (doctor
   gascity-city-configuration-doctor
   (default 'unset))
  (dolt
   gascity-city-configuration-dolt
   (default 'unset))
  (events
   gascity-city-configuration-events
   (default 'unset))
  (extmsg
   gascity-city-configuration-extmsg
   (default 'unset))
  (formulas
   gascity-city-configuration-formulas
   (default 'unset))
  (github
   gascity-city-configuration-github
   (default 'unset))
  (imports
   gascity-city-configuration-imports
   (default 'unset))
  (include
   gascity-city-configuration-include
   (default 'unset))
  (mail
   gascity-city-configuration-mail
   (default 'unset))
  (maintenance
   gascity-city-configuration-maintenance
   (default 'unset))
  (named-session
   gascity-city-configuration-named-session
   (default 'unset))
  (orders
   gascity-city-configuration-orders
   (default 'unset))
  (patches
   gascity-city-configuration-patches
   (default 'unset))
  (pricing
   gascity-city-configuration-pricing
   (default 'unset))
  (providers
   gascity-city-configuration-providers
   (default 'unset))
  (rigs
   gascity-city-configuration-rigs
   (default 'unset))
  (service
   gascity-city-configuration-service
   (default 'unset))
  (session
   gascity-city-configuration-session
   (default 'unset))
  (session-sleep
   gascity-city-configuration-session-sleep
   (default 'unset))
  (storage
   gascity-city-configuration-storage
   (default 'unset))
  (upstreams
   gascity-city-configuration-upstreams
   (default 'unset))
  (usage
   gascity-city-configuration-usage
   (default 'unset))
  (webhook
   gascity-city-configuration-webhook
   (default 'unset))
  (webhooks
   gascity-city-configuration-webhooks
   (default 'unset))
  (workspace
   gascity-city-configuration-workspace
   (default 'unset))
  )

(define (gascity-city-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-city-configuration>."
  (match-record config <gascity-city-configuration>
    (agent agent-defaults api beads chat-sessions convergence daemon defaults doctor dolt events extmsg formulas github imports include mail maintenance named-session orders patches pricing providers rigs service session session-sleep storage upstreams usage webhook webhooks workspace)
    (append
     (gascity-records-serialize-subtables "agent" agent gascity-agent-configuration->toml)
     (gascity-records-serialize-subtable "agent_defaults" agent-defaults gascity-agent-defaults-configuration->toml)
     (gascity-records-serialize-subtable "api" api gascity-api-configuration->toml)
     (gascity-records-serialize-subtable "beads" beads gascity-beads-configuration->toml)
     (gascity-records-serialize-subtable "chat_sessions" chat-sessions gascity-chat-sessions-configuration->toml)
     (gascity-records-serialize-subtable "convergence" convergence gascity-convergence-configuration->toml)
     (gascity-records-serialize-subtable "daemon" daemon gascity-daemon-configuration->toml)
     (gascity-records-serialize-subtable "defaults" defaults gascity-pack-defaults-configuration->toml)
     (gascity-records-serialize-subtable "doctor" doctor gascity-doctor-configuration->toml)
     (gascity-records-serialize-subtable "dolt" dolt gascity-dolt-configuration->toml)
     (gascity-records-serialize-subtable "events" events gascity-events-configuration->toml)
     (gascity-records-serialize-subtable "extmsg" extmsg gascity-ext-msg-configuration->toml)
     (gascity-records-serialize-subtable "formulas" formulas gascity-formulas-configuration->toml)
     (gascity-records-serialize-subtable "github" github gascity-github-configuration->toml)
     (gascity-records-serialize-alist-subtables "imports" imports gascity-import-configuration->toml)
     (gascity-records-serialize-list-of-strings "include" include)
     (gascity-records-serialize-subtable "mail" mail gascity-mail-configuration->toml)
     (gascity-records-serialize-subtable "maintenance" maintenance gascity-maintenance-configuration->toml)
     (gascity-records-serialize-subtables "named_session" named-session gascity-named-session-configuration->toml)
     (gascity-records-serialize-subtable "orders" orders gascity-orders-configuration->toml)
     (gascity-records-serialize-subtable "patches" patches gascity-patches-configuration->toml)
     (gascity-records-serialize-subtables "pricing" pricing gascity-model-pricing-configuration->toml)
     (gascity-records-serialize-alist-subtables "providers" providers gascity-provider-spec-configuration->toml)
     (gascity-records-serialize-subtables "rigs" rigs gascity-rig-configuration->toml)
     (gascity-records-serialize-subtables "service" service gascity-service-configuration->toml)
     (gascity-records-serialize-subtable "session" session gascity-session-configuration->toml)
     (gascity-records-serialize-subtable "session_sleep" session-sleep gascity-session-sleep-configuration->toml)
     (gascity-records-serialize-subtable "storage" storage gascity-storage-configuration->toml)
     (gascity-records-serialize-alist-subtables "upstreams" upstreams gascity-upstream-spec-configuration->toml)
     (gascity-records-serialize-subtable "usage" usage gascity-usage-configuration->toml)
     (gascity-records-serialize-subtables "webhook" webhook gascity-webhook-configuration->toml)
     (gascity-records-serialize-subtable "webhooks" webhooks gascity-webhook-policy-configuration->toml)
     (gascity-records-serialize-subtable "workspace" workspace gascity-workspace-configuration->toml)
     '())))

;; ContextAdvisory — from `$defs.ContextAdvisory'.

(define-record-type* <gascity-context-advisory-configuration>
  gascity-context-advisory-configuration
  make-gascity-context-advisory-configuration
  gascity-context-advisory-configuration?
  (enabled
   gascity-context-advisory-configuration-enabled
   (default 'unset))
  (tiers
   gascity-context-advisory-configuration-tiers
   (default 'unset))
  (window-tokens
   gascity-context-advisory-configuration-window-tokens
   (default 'unset))
  )

(define (gascity-context-advisory-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-context-advisory-configuration>."
  (match-record config <gascity-context-advisory-configuration>
    (enabled tiers window-tokens)
    (append
     (gascity-records-serialize-tri-state "enabled" enabled)
     (gascity-records-serialize-subtables "tiers" tiers gascity-context-advisory-tier-configuration->toml)
     (gascity-records-serialize-maybe-integer "window_tokens" window-tokens)
     '())))

;; ContextAdvisoryTier — from `$defs.ContextAdvisoryTier'.

(define-record-type* <gascity-context-advisory-tier-configuration>
  gascity-context-advisory-tier-configuration
  make-gascity-context-advisory-tier-configuration
  gascity-context-advisory-tier-configuration?
  (enabled
   gascity-context-advisory-tier-configuration-enabled
   (default 'unset))
  (message
   gascity-context-advisory-tier-configuration-message
   (default 'unset))
  (threshold
   gascity-context-advisory-tier-configuration-threshold
   (default 'unset))
  )

(define (gascity-context-advisory-tier-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-context-advisory-tier-configuration>."
  (match-record config <gascity-context-advisory-tier-configuration>
    (enabled message threshold)
    (append
     (gascity-records-serialize-tri-state "enabled" enabled)
     (gascity-records-serialize-maybe-string "message" message)
     (gascity-records-serialize-maybe-integer "threshold" threshold)
     '())))

;; ConvergenceConfig — from `$defs.ConvergenceConfig'.

(define-record-type* <gascity-convergence-configuration>
  gascity-convergence-configuration
  make-gascity-convergence-configuration
  gascity-convergence-configuration?
  (max-per-agent
   gascity-convergence-configuration-max-per-agent
   (default 'unset))
  (max-total
   gascity-convergence-configuration-max-total
   (default 'unset))
  )

(define (gascity-convergence-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-convergence-configuration>."
  (match-record config <gascity-convergence-configuration>
    (max-per-agent max-total)
    (append
     (gascity-records-serialize-maybe-integer "max_per_agent" max-per-agent)
     (gascity-records-serialize-maybe-integer "max_total" max-total)
     '())))

;; DaemonConfig — from `$defs.DaemonConfig'.

(define-record-type* <gascity-daemon-configuration>
  gascity-daemon-configuration
  make-gascity-daemon-configuration
  gascity-daemon-configuration?
  (auto-prune-worker-dir
   gascity-daemon-configuration-auto-prune-worker-dir
   (default 'unset))
  (auto-reap-closed-bead-worktrees
   gascity-daemon-configuration-auto-reap-closed-bead-worktrees
   (default 'unset))
  (auto-reap-closed-bead-worktrees-dry-run
   gascity-daemon-configuration-auto-reap-closed-bead-worktrees-dry-run
   (default 'unset))
  (auto-reap-closed-bead-worktrees-min-age-minutes
   gascity-daemon-configuration-auto-reap-closed-bead-worktrees-min-age-minutes
   (default 'unset))
  (auto-restart-on-drift
   gascity-daemon-configuration-auto-restart-on-drift
   (default 'unset))
  (dolt-start-address-in-use-retry-window
   gascity-daemon-configuration-dolt-start-address-in-use-retry-window
   (default 'unset))
  (dolt-stop-timeout
   gascity-daemon-configuration-dolt-stop-timeout
   (default 'unset))
  (drift-drain-timeout
   gascity-daemon-configuration-drift-drain-timeout
   (default 'unset))
  (formula-v2
   gascity-daemon-configuration-formula-v2
   (default 'unset))
  (graph-workflows
   gascity-daemon-configuration-graph-workflows
   (default 'unset))
  (max-restarts
   gascity-daemon-configuration-max-restarts
   (default 'unset))
  (max-wakes-per-tick
   gascity-daemon-configuration-max-wakes-per-tick
   (default 'unset))
  (nudge-dispatcher
   gascity-daemon-configuration-nudge-dispatcher
   (default 'unset))
  (observe-paths
   gascity-daemon-configuration-observe-paths
   (default 'unset))
  (patrol-interval
   gascity-daemon-configuration-patrol-interval
   (default 'unset))
  (probe-concurrency
   gascity-daemon-configuration-probe-concurrency
   (default 'unset))
  (restart-window
   gascity-daemon-configuration-restart-window
   (default 'unset))
  (session-circuit-breaker
   gascity-daemon-configuration-session-circuit-breaker
   (default 'unset))
  (session-circuit-breaker-max-restarts
   gascity-daemon-configuration-session-circuit-breaker-max-restarts
   (default 'unset))
  (session-circuit-breaker-reset-after
   gascity-daemon-configuration-session-circuit-breaker-reset-after
   (default 'unset))
  (session-circuit-breaker-window
   gascity-daemon-configuration-session-circuit-breaker-window
   (default 'unset))
  (shutdown-timeout
   gascity-daemon-configuration-shutdown-timeout
   (default 'unset))
  (start-ready-timeout
   gascity-daemon-configuration-start-ready-timeout
   (default 'unset))
  (tick-debounce
   gascity-daemon-configuration-tick-debounce
   (default 'unset))
  (wisp-gc-interval
   gascity-daemon-configuration-wisp-gc-interval
   (default 'unset))
  (wisp-ttl
   gascity-daemon-configuration-wisp-ttl
   (default 'unset))
  )

(define (gascity-daemon-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-daemon-configuration>."
  (match-record config <gascity-daemon-configuration>
    (auto-prune-worker-dir auto-reap-closed-bead-worktrees auto-reap-closed-bead-worktrees-dry-run auto-reap-closed-bead-worktrees-min-age-minutes auto-restart-on-drift dolt-start-address-in-use-retry-window dolt-stop-timeout drift-drain-timeout formula-v2 graph-workflows max-restarts max-wakes-per-tick nudge-dispatcher observe-paths patrol-interval probe-concurrency restart-window session-circuit-breaker session-circuit-breaker-max-restarts session-circuit-breaker-reset-after session-circuit-breaker-window shutdown-timeout start-ready-timeout tick-debounce wisp-gc-interval wisp-ttl)
    (append
     (gascity-records-serialize-tri-state "auto_prune_worker_dir" auto-prune-worker-dir)
     (gascity-records-serialize-tri-state "auto_reap_closed_bead_worktrees" auto-reap-closed-bead-worktrees)
     (gascity-records-serialize-tri-state "auto_reap_closed_bead_worktrees_dry_run" auto-reap-closed-bead-worktrees-dry-run)
     (gascity-records-serialize-maybe-integer "auto_reap_closed_bead_worktrees_min_age_minutes" auto-reap-closed-bead-worktrees-min-age-minutes)
     (gascity-records-serialize-tri-state "auto_restart_on_drift" auto-restart-on-drift)
     (gascity-records-serialize-maybe-string "dolt_start_address_in_use_retry_window" dolt-start-address-in-use-retry-window)
     (gascity-records-serialize-maybe-string "dolt_stop_timeout" dolt-stop-timeout)
     (gascity-records-serialize-maybe-string "drift_drain_timeout" drift-drain-timeout)
     (gascity-records-serialize-tri-state "formula_v2" formula-v2)
     (gascity-records-serialize-tri-state "graph_workflows" graph-workflows)
     (gascity-records-serialize-maybe-integer "max_restarts" max-restarts)
     (gascity-records-serialize-maybe-integer "max_wakes_per_tick" max-wakes-per-tick)
     (gascity-records-serialize-maybe-string "nudge_dispatcher" nudge-dispatcher)
     (gascity-records-serialize-list-of-strings "observe_paths" observe-paths)
     (gascity-records-serialize-maybe-string "patrol_interval" patrol-interval)
     (gascity-records-serialize-maybe-integer "probe_concurrency" probe-concurrency)
     (gascity-records-serialize-maybe-string "restart_window" restart-window)
     (gascity-records-serialize-tri-state "session_circuit_breaker" session-circuit-breaker)
     (gascity-records-serialize-maybe-integer "session_circuit_breaker_max_restarts" session-circuit-breaker-max-restarts)
     (gascity-records-serialize-maybe-string "session_circuit_breaker_reset_after" session-circuit-breaker-reset-after)
     (gascity-records-serialize-maybe-string "session_circuit_breaker_window" session-circuit-breaker-window)
     (gascity-records-serialize-maybe-string "shutdown_timeout" shutdown-timeout)
     (gascity-records-serialize-maybe-string "start_ready_timeout" start-ready-timeout)
     (gascity-records-serialize-maybe-string "tick_debounce" tick-debounce)
     (gascity-records-serialize-maybe-string "wisp_gc_interval" wisp-gc-interval)
     (gascity-records-serialize-maybe-string "wisp_ttl" wisp-ttl)
     '())))

;; DoctorConfig — from `$defs.DoctorConfig'.

(define-record-type* <gascity-doctor-configuration>
  gascity-doctor-configuration
  make-gascity-doctor-configuration
  gascity-doctor-configuration?
  (check
   gascity-doctor-configuration-check
   (default 'unset))
  (nested-worktree-prune
   gascity-doctor-configuration-nested-worktree-prune
   (default 'unset))
  (worktree-rig-error-size
   gascity-doctor-configuration-worktree-rig-error-size
   (default 'unset))
  (worktree-rig-warn-size
   gascity-doctor-configuration-worktree-rig-warn-size
   (default 'unset))
  )

(define (gascity-doctor-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-doctor-configuration>."
  (match-record config <gascity-doctor-configuration>
    (check nested-worktree-prune worktree-rig-error-size worktree-rig-warn-size)
    (append
     (gascity-records-serialize-subtables "check" check gascity-local-doctor-check-configuration->toml)
     (gascity-records-serialize-tri-state "nested_worktree_prune" nested-worktree-prune)
     (gascity-records-serialize-maybe-string "worktree_rig_error_size" worktree-rig-error-size)
     (gascity-records-serialize-maybe-string "worktree_rig_warn_size" worktree-rig-warn-size)
     '())))

;; DoltConfig — from `$defs.DoltConfig'.

(define-record-type* <gascity-dolt-configuration>
  gascity-dolt-configuration
  make-gascity-dolt-configuration
  gascity-dolt-configuration?
  (archive-level
   gascity-dolt-configuration-archive-level
   (default 'unset))
  (auto-gc-enabled
   gascity-dolt-configuration-auto-gc-enabled
   (default 'unset))
  (dolt-lock-release-timeout
   gascity-dolt-configuration-dolt-lock-release-timeout
   (default 'unset))
  (host
   gascity-dolt-configuration-host
   (default 'unset))
  (max-connections
   gascity-dolt-configuration-max-connections
   (default 'unset))
  (port
   gascity-dolt-configuration-port
   (default 'unset))
  (read-timeout-millis
   gascity-dolt-configuration-read-timeout-millis
   (default 'unset))
  (wait-timeout-seconds
   gascity-dolt-configuration-wait-timeout-seconds
   (default 'unset))
  (write-timeout-millis
   gascity-dolt-configuration-write-timeout-millis
   (default 'unset))
  )

(define (gascity-dolt-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-dolt-configuration>."
  (match-record config <gascity-dolt-configuration>
    (archive-level auto-gc-enabled dolt-lock-release-timeout host max-connections port read-timeout-millis wait-timeout-seconds write-timeout-millis)
    (append
     (gascity-records-serialize-maybe-integer "archive_level" archive-level)
     (gascity-records-serialize-tri-state "auto_gc_enabled" auto-gc-enabled)
     (gascity-records-serialize-maybe-string "dolt_lock_release_timeout" dolt-lock-release-timeout)
     (gascity-records-serialize-maybe-string "host" host)
     (gascity-records-serialize-maybe-integer "max_connections" max-connections)
     (gascity-records-serialize-maybe-integer "port" port)
     (gascity-records-serialize-maybe-integer "read_timeout_millis" read-timeout-millis)
     (gascity-records-serialize-maybe-integer "wait_timeout_seconds" wait-timeout-seconds)
     (gascity-records-serialize-maybe-integer "write_timeout_millis" write-timeout-millis)
     '())))

;; DoltMaintenance — from `$defs.DoltMaintenance'.

(define-record-type* <gascity-dolt-maintenance-configuration>
  gascity-dolt-maintenance-configuration
  make-gascity-dolt-maintenance-configuration
  gascity-dolt-maintenance-configuration?
  (alert-to
   gascity-dolt-maintenance-configuration-alert-to
   (default 'unset))
  (enabled
   gascity-dolt-maintenance-configuration-enabled
   (default 'unset))
  (gc-timeout
   gascity-dolt-maintenance-configuration-gc-timeout
   (default 'unset))
  (interval
   gascity-dolt-maintenance-configuration-interval
   (default 'unset))
  )

(define (gascity-dolt-maintenance-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-dolt-maintenance-configuration>."
  (match-record config <gascity-dolt-maintenance-configuration>
    (alert-to enabled gc-timeout interval)
    (append
     (gascity-records-serialize-maybe-string "alert_to" alert-to)
     (gascity-records-serialize-tri-state "enabled" enabled)
     (gascity-records-serialize-maybe-string "gc_timeout" gc-timeout)
     (gascity-records-serialize-maybe-string "interval" interval)
     '())))

;; EventsConfig — from `$defs.EventsConfig'.

(define-record-type* <gascity-events-configuration>
  gascity-events-configuration
  make-gascity-events-configuration
  gascity-events-configuration?
  (provider
   gascity-events-configuration-provider
   (default 'unset))
  (rotation
   gascity-events-configuration-rotation
   (default 'unset))
  )

(define (gascity-events-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-events-configuration>."
  (match-record config <gascity-events-configuration>
    (provider rotation)
    (append
     (gascity-records-serialize-maybe-string "provider" provider)
     (gascity-records-serialize-subtable "rotation" rotation gascity-events-rotation-configuration->toml)
     '())))

;; EventsRotationConfig — from `$defs.EventsRotationConfig'.

(define-record-type* <gascity-events-rotation-configuration>
  gascity-events-rotation-configuration
  make-gascity-events-rotation-configuration
  gascity-events-rotation-configuration?
  (archive-retain-age
   gascity-events-rotation-configuration-archive-retain-age
   (default 'unset))
  (check-interval-records
   gascity-events-rotation-configuration-check-interval-records
   (default 'unset))
  (check-interval-seconds
   gascity-events-rotation-configuration-check-interval-seconds
   (default 'unset))
  (enabled
   gascity-events-rotation-configuration-enabled
   (default 'unset))
  (max-size-bytes
   gascity-events-rotation-configuration-max-size-bytes
   (default 'unset))
  )

(define (gascity-events-rotation-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-events-rotation-configuration>."
  (match-record config <gascity-events-rotation-configuration>
    (archive-retain-age check-interval-records check-interval-seconds enabled max-size-bytes)
    (append
     (gascity-records-serialize-maybe-string "archive_retain_age" archive-retain-age)
     (gascity-records-serialize-maybe-integer "check_interval_records" check-interval-records)
     (gascity-records-serialize-maybe-integer "check_interval_seconds" check-interval-seconds)
     (gascity-records-serialize-tri-state "enabled" enabled)
     (gascity-records-serialize-maybe-integer "max_size_bytes" max-size-bytes)
     '())))

;; ExtMsgConfig — from `$defs.ExtMsgConfig'.

(define-record-type* <gascity-ext-msg-configuration>
  gascity-ext-msg-configuration
  make-gascity-ext-msg-configuration
  gascity-ext-msg-configuration?
  (default-route
   gascity-ext-msg-configuration-default-route
   (default 'unset))
  )

(define (gascity-ext-msg-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-ext-msg-configuration>."
  (match-record config <gascity-ext-msg-configuration>
    (default-route)
    (append
     (gascity-records-serialize-subtables "default_route" default-route gascity-ext-msg-default-route-configuration->toml)
     '())))

;; ExtMsgDefaultRoute — from `$defs.ExtMsgDefaultRoute'.

(define-record-type* <gascity-ext-msg-default-route-configuration>
  gascity-ext-msg-default-route-configuration
  make-gascity-ext-msg-default-route-configuration
  gascity-ext-msg-default-route-configuration?
  (account-id
   gascity-ext-msg-default-route-configuration-account-id
   (default 'unset))
  (agent
   gascity-ext-msg-default-route-configuration-agent
   (default 'unset))
  (provider
   gascity-ext-msg-default-route-configuration-provider
   (default 'unset))
  )

(define (gascity-ext-msg-default-route-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-ext-msg-default-route-configuration>."
  (match-record config <gascity-ext-msg-default-route-configuration>
    (account-id agent provider)
    (append
     (gascity-records-serialize-maybe-string "account_id" account-id)
     (gascity-records-serialize-maybe-string "agent" agent)
     (gascity-records-serialize-maybe-string "provider" provider)
     '())))

;; FormulasConfig — from `$defs.FormulasConfig'.

(define-record-type* <gascity-formulas-configuration>
  gascity-formulas-configuration
  make-gascity-formulas-configuration
  gascity-formulas-configuration?
  )

(define (gascity-formulas-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-formulas-configuration>."
  (match-record config <gascity-formulas-configuration>
    ()
    '()))

;; GitHubConfig — from `$defs.GitHubConfig'.

(define-record-type* <gascity-github-configuration>
  gascity-github-configuration
  make-gascity-github-configuration
  gascity-github-configuration?
  (pr-monitor
   gascity-github-configuration-pr-monitor
   (default 'unset))
  )

(define (gascity-github-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-github-configuration>."
  (match-record config <gascity-github-configuration>
    (pr-monitor)
    (append
     (gascity-records-serialize-subtables "pr_monitor" pr-monitor gascity-github-pr-monitor-configuration->toml)
     '())))

;; GitHubPRMonitor — from `$defs.GitHubPRMonitor'.

(define-record-type* <gascity-github-pr-monitor-configuration>
  gascity-github-pr-monitor-configuration
  make-gascity-github-pr-monitor-configuration
  gascity-github-pr-monitor-configuration?
  (base-branches
   gascity-github-pr-monitor-configuration-base-branches
   (default 'unset))
  (merge-queue
   gascity-github-pr-monitor-configuration-merge-queue
   (default 'unset))
  (name
   gascity-github-pr-monitor-configuration-name
   (default 'unset))
  (notify
   gascity-github-pr-monitor-configuration-notify
   (default 'unset))
  (owner
   gascity-github-pr-monitor-configuration-owner
   (default 'unset))
  (poll-interval
   gascity-github-pr-monitor-configuration-poll-interval
   (default 'unset))
  (repair-route
   gascity-github-pr-monitor-configuration-repair-route
   (default 'unset))
  (repair-workflow
   gascity-github-pr-monitor-configuration-repair-workflow
   (default 'unset))
  (repo
   gascity-github-pr-monitor-configuration-repo
   (default 'unset))
  (rig
   gascity-github-pr-monitor-configuration-rig
   (default 'unset))
  (webhook-secret-env
   gascity-github-pr-monitor-configuration-webhook-secret-env
   (default 'unset))
  (webhook-secret-key
   gascity-github-pr-monitor-configuration-webhook-secret-key
   (default 'unset))
  )

(define (gascity-github-pr-monitor-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-github-pr-monitor-configuration>."
  (match-record config <gascity-github-pr-monitor-configuration>
    (base-branches merge-queue name notify owner poll-interval repair-route repair-workflow repo rig webhook-secret-env webhook-secret-key)
    (append
     (gascity-records-serialize-list-of-strings "base_branches" base-branches)
     (gascity-records-serialize-maybe-string "merge_queue" merge-queue)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-list-of-strings "notify" notify)
     (gascity-records-serialize-maybe-string "owner" owner)
     (gascity-records-serialize-maybe-string "poll_interval" poll-interval)
     (gascity-records-serialize-maybe-string "repair_route" repair-route)
     (gascity-records-serialize-maybe-string "repair_workflow" repair-workflow)
     (gascity-records-serialize-maybe-string "repo" repo)
     (gascity-records-serialize-maybe-string "rig" rig)
     (gascity-records-serialize-maybe-string "webhook_secret_env" webhook-secret-env)
     (gascity-records-serialize-maybe-string "webhook_secret_key" webhook-secret-key)
     '())))

;; GitHubPRMonitorPatch — from `$defs.GitHubPRMonitorPatch'.

(define-record-type* <gascity-github-pr-monitor-patch-configuration>
  gascity-github-pr-monitor-patch-configuration
  make-gascity-github-pr-monitor-patch-configuration
  gascity-github-pr-monitor-patch-configuration?
  (base-branches
   gascity-github-pr-monitor-patch-configuration-base-branches
   (default 'unset))
  (merge-queue
   gascity-github-pr-monitor-patch-configuration-merge-queue
   (default 'unset))
  (name
   gascity-github-pr-monitor-patch-configuration-name
   (default 'unset))
  (notify
   gascity-github-pr-monitor-patch-configuration-notify
   (default 'unset))
  (notify-append
   gascity-github-pr-monitor-patch-configuration-notify-append
   (default 'unset))
  (owner
   gascity-github-pr-monitor-patch-configuration-owner
   (default 'unset))
  (poll-interval
   gascity-github-pr-monitor-patch-configuration-poll-interval
   (default 'unset))
  (repair-route
   gascity-github-pr-monitor-patch-configuration-repair-route
   (default 'unset))
  (repair-workflow
   gascity-github-pr-monitor-patch-configuration-repair-workflow
   (default 'unset))
  (repo
   gascity-github-pr-monitor-patch-configuration-repo
   (default 'unset))
  (rig
   gascity-github-pr-monitor-patch-configuration-rig
   (default 'unset))
  (webhook-secret-env
   gascity-github-pr-monitor-patch-configuration-webhook-secret-env
   (default 'unset))
  (webhook-secret-key
   gascity-github-pr-monitor-patch-configuration-webhook-secret-key
   (default 'unset))
  )

(define (gascity-github-pr-monitor-patch-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-github-pr-monitor-patch-configuration>."
  (match-record config <gascity-github-pr-monitor-patch-configuration>
    (base-branches merge-queue name notify notify-append owner poll-interval repair-route repair-workflow repo rig webhook-secret-env webhook-secret-key)
    (append
     (gascity-records-serialize-list-of-strings "base_branches" base-branches)
     (gascity-records-serialize-maybe-string "merge_queue" merge-queue)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-list-of-strings "notify" notify)
     (gascity-records-serialize-list-of-strings "notify_append" notify-append)
     (gascity-records-serialize-maybe-string "owner" owner)
     (gascity-records-serialize-maybe-string "poll_interval" poll-interval)
     (gascity-records-serialize-maybe-string "repair_route" repair-route)
     (gascity-records-serialize-maybe-string "repair_workflow" repair-workflow)
     (gascity-records-serialize-maybe-string "repo" repo)
     (gascity-records-serialize-maybe-string "rig" rig)
     (gascity-records-serialize-maybe-string "webhook_secret_env" webhook-secret-env)
     (gascity-records-serialize-maybe-string "webhook_secret_key" webhook-secret-key)
     '())))

;; Import — from `$defs.Import'.

(define-record-type* <gascity-import-configuration>
  gascity-import-configuration
  make-gascity-import-configuration
  gascity-import-configuration?
  (source
   gascity-import-configuration-source
   (default 'unset))
  (version
   gascity-import-configuration-version
   (default 'unset))
  )

(define (gascity-import-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-import-configuration>."
  (match-record config <gascity-import-configuration>
    (source version)
    (append
     (gascity-records-serialize-maybe-string "source" source)
     (gascity-records-serialize-maybe-string "version" version)
     '())))

;; K8sConfig — from `$defs.K8sConfig'.

(define-record-type* <gascity-k8s-configuration>
  gascity-k8s-configuration
  make-gascity-k8s-configuration
  gascity-k8s-configuration?
  (context
   gascity-k8s-configuration-context
   (default 'unset))
  (cpu-limit
   gascity-k8s-configuration-cpu-limit
   (default 'unset))
  (cpu-request
   gascity-k8s-configuration-cpu-request
   (default 'unset))
  (image
   gascity-k8s-configuration-image
   (default 'unset))
  (mem-limit
   gascity-k8s-configuration-mem-limit
   (default 'unset))
  (mem-request
   gascity-k8s-configuration-mem-request
   (default 'unset))
  (namespace
   gascity-k8s-configuration-namespace
   (default 'unset))
  (prebaked
   gascity-k8s-configuration-prebaked
   (default 'unset))
  )

(define (gascity-k8s-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-k8s-configuration>."
  (match-record config <gascity-k8s-configuration>
    (context cpu-limit cpu-request image mem-limit mem-request namespace prebaked)
    (append
     (gascity-records-serialize-maybe-string "context" context)
     (gascity-records-serialize-maybe-string "cpu_limit" cpu-limit)
     (gascity-records-serialize-maybe-string "cpu_request" cpu-request)
     (gascity-records-serialize-maybe-string "image" image)
     (gascity-records-serialize-maybe-string "mem_limit" mem-limit)
     (gascity-records-serialize-maybe-string "mem_request" mem-request)
     (gascity-records-serialize-maybe-string "namespace" namespace)
     (gascity-records-serialize-tri-state "prebaked" prebaked)
     '())))

;; LocalDoctorCheck — from `$defs.LocalDoctorCheck'.

(define-record-type* <gascity-local-doctor-check-configuration>
  gascity-local-doctor-check-configuration
  make-gascity-local-doctor-check-configuration
  gascity-local-doctor-check-configuration?
  (description
   gascity-local-doctor-check-configuration-description
   (default 'unset))
  (fix
   gascity-local-doctor-check-configuration-fix
   (default 'unset))
  (name
   gascity-local-doctor-check-configuration-name
   (default 'unset))
  (script
   gascity-local-doctor-check-configuration-script
   (default 'unset))
  )

(define (gascity-local-doctor-check-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-local-doctor-check-configuration>."
  (match-record config <gascity-local-doctor-check-configuration>
    (description fix name script)
    (append
     (gascity-records-serialize-maybe-string "description" description)
     (gascity-records-serialize-string-or-gexp "fix" fix)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-string-or-gexp "script" script)
     '())))

;; MailConfig — from `$defs.MailConfig'.

(define-record-type* <gascity-mail-configuration>
  gascity-mail-configuration
  make-gascity-mail-configuration
  gascity-mail-configuration?
  (provider
   gascity-mail-configuration-provider
   (default 'unset))
  (retention-ttl
   gascity-mail-configuration-retention-ttl
   (default 'unset))
  )

(define (gascity-mail-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-mail-configuration>."
  (match-record config <gascity-mail-configuration>
    (provider retention-ttl)
    (append
     (gascity-records-serialize-maybe-string "provider" provider)
     (gascity-records-serialize-maybe-string "retention_ttl" retention-ttl)
     '())))

;; MaintenanceConfig — from `$defs.MaintenanceConfig'.

(define-record-type* <gascity-maintenance-configuration>
  gascity-maintenance-configuration
  make-gascity-maintenance-configuration
  gascity-maintenance-configuration?
  (dolt
   gascity-maintenance-configuration-dolt
   (default 'unset))
  )

(define (gascity-maintenance-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-maintenance-configuration>."
  (match-record config <gascity-maintenance-configuration>
    (dolt)
    (append
     (gascity-records-serialize-subtable "dolt" dolt gascity-dolt-maintenance-configuration->toml)
     '())))

;; ModelPricing — from `$defs.ModelPricing'.

(define-record-type* <gascity-model-pricing-configuration>
  gascity-model-pricing-configuration
  make-gascity-model-pricing-configuration
  gascity-model-pricing-configuration?
  (last-verified
   gascity-model-pricing-configuration-last-verified
   (default 'unset))
  (model
   gascity-model-pricing-configuration-model
   (default 'unset))
  (provider
   gascity-model-pricing-configuration-provider
   (default 'unset))
  (tier
   gascity-model-pricing-configuration-tier
   (default 'unset))
  )

(define (gascity-model-pricing-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-model-pricing-configuration>."
  (match-record config <gascity-model-pricing-configuration>
    (last-verified model provider tier)
    (append
     (gascity-records-serialize-maybe-string "last_verified" last-verified)
     (gascity-records-serialize-maybe-string "model" model)
     (gascity-records-serialize-maybe-string "provider" provider)
     (gascity-records-serialize-subtable "tier" tier gascity-tier-configuration->toml)
     '())))

;; NamedSession — from `$defs.NamedSession'.

(define-record-type* <gascity-named-session-configuration>
  gascity-named-session-configuration
  make-gascity-named-session-configuration
  gascity-named-session-configuration?
  (dir
   gascity-named-session-configuration-dir
   (default 'unset))
  (mode
   gascity-named-session-configuration-mode
   (default 'unset))
  (name
   gascity-named-session-configuration-name
   (default 'unset))
  (scope
   gascity-named-session-configuration-scope
   (default 'unset))
  (template
   gascity-named-session-configuration-template
   (default 'unset))
  )

(define (gascity-named-session-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-named-session-configuration>."
  (match-record config <gascity-named-session-configuration>
    (dir mode name scope template)
    (append
     (gascity-records-serialize-maybe-string "dir" dir)
     (gascity-records-serialize-maybe-string "mode" mode)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-maybe-string "scope" scope)
     (gascity-records-serialize-maybe-string "template" template)
     '())))

;; NamedSessionPatch — from `$defs.NamedSessionPatch'.

(define-record-type* <gascity-named-session-patch-configuration>
  gascity-named-session-patch-configuration
  make-gascity-named-session-patch-configuration
  gascity-named-session-patch-configuration?
  (dir
   gascity-named-session-patch-configuration-dir
   (default 'unset))
  (mode
   gascity-named-session-patch-configuration-mode
   (default 'unset))
  (name
   gascity-named-session-patch-configuration-name
   (default 'unset))
  (template
   gascity-named-session-patch-configuration-template
   (default 'unset))
  )

(define (gascity-named-session-patch-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-named-session-patch-configuration>."
  (match-record config <gascity-named-session-patch-configuration>
    (dir mode name template)
    (append
     (gascity-records-serialize-maybe-string "dir" dir)
     (gascity-records-serialize-maybe-string "mode" mode)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-maybe-string "template" template)
     '())))

;; OptionChoice — from `$defs.OptionChoice'.

(define-record-type* <gascity-option-choice-configuration>
  gascity-option-choice-configuration
  make-gascity-option-choice-configuration
  gascity-option-choice-configuration?
  (flag-aliases
   gascity-option-choice-configuration-flag-aliases
   (default 'unset))
  (flag-args
   gascity-option-choice-configuration-flag-args
   (default 'unset))
  (label
   gascity-option-choice-configuration-label
   (default 'unset))
  (value
   gascity-option-choice-configuration-value
   (default 'unset))
  )

(define (gascity-option-choice-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-option-choice-configuration>."
  (match-record config <gascity-option-choice-configuration>
    (flag-aliases flag-args label value)
    (append
     (gascity-records-serialize-list-of-lists-of-strings "flag_aliases" flag-aliases)
     (gascity-records-serialize-list-of-strings "flag_args" flag-args)
     (gascity-records-serialize-maybe-string "label" label)
     (gascity-records-serialize-maybe-string "value" value)
     '())))

;; OrderOverride — from `$defs.OrderOverride'.

(define-record-type* <gascity-order-override-configuration>
  gascity-order-override-configuration
  make-gascity-order-override-configuration
  gascity-order-override-configuration?
  (check
   gascity-order-override-configuration-check
   (default 'unset))
  (check-timeout
   gascity-order-override-configuration-check-timeout
   (default 'unset))
  (enabled
   gascity-order-override-configuration-enabled
   (default 'unset))
  (env
   gascity-order-override-configuration-env
   (default 'unset))
  (gate
   gascity-order-override-configuration-gate
   (default 'unset))
  (idempotent
   gascity-order-override-configuration-idempotent
   (default 'unset))
  (interval
   gascity-order-override-configuration-interval
   (default 'unset))
  (name
   gascity-order-override-configuration-name
   (default 'unset))
  (on
   gascity-order-override-configuration-on
   (default 'unset))
  (pool
   gascity-order-override-configuration-pool
   (default 'unset))
  (rig
   gascity-order-override-configuration-rig
   (default 'unset))
  (schedule
   gascity-order-override-configuration-schedule
   (default 'unset))
  (timeout
   gascity-order-override-configuration-timeout
   (default 'unset))
  (trigger
   gascity-order-override-configuration-trigger
   (default 'unset))
  )

(define (gascity-order-override-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-order-override-configuration>."
  (match-record config <gascity-order-override-configuration>
    (check check-timeout enabled env gate idempotent interval name on pool rig schedule timeout trigger)
    (append
     (gascity-records-serialize-string-or-gexp "check" check)
     (gascity-records-serialize-maybe-string "check_timeout" check-timeout)
     (gascity-records-serialize-tri-state "enabled" enabled)
     (gascity-records-serialize-string-map "env" env)
     (gascity-records-serialize-maybe-string "gate" gate)
     (gascity-records-serialize-tri-state "idempotent" idempotent)
     (gascity-records-serialize-maybe-string "interval" interval)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-maybe-string "on" on)
     (gascity-records-serialize-maybe-string "pool" pool)
     (gascity-records-serialize-maybe-string "rig" rig)
     (gascity-records-serialize-maybe-string "schedule" schedule)
     (gascity-records-serialize-maybe-string "timeout" timeout)
     (gascity-records-serialize-maybe-string "trigger" trigger)
     '())))

;; OrdersConfig — from `$defs.OrdersConfig'.

(define-record-type* <gascity-orders-configuration>
  gascity-orders-configuration
  make-gascity-orders-configuration
  gascity-orders-configuration?
  (max-dispatches-per-tick
   gascity-orders-configuration-max-dispatches-per-tick
   (default 'unset))
  (max-timeout
   gascity-orders-configuration-max-timeout
   (default 'unset))
  (overrides
   gascity-orders-configuration-overrides
   (default 'unset))
  (skip
   gascity-orders-configuration-skip
   (default 'unset))
  )

(define (gascity-orders-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-orders-configuration>."
  (match-record config <gascity-orders-configuration>
    (max-dispatches-per-tick max-timeout overrides skip)
    (append
     (gascity-records-serialize-maybe-integer "max_dispatches_per_tick" max-dispatches-per-tick)
     (gascity-records-serialize-maybe-string "max_timeout" max-timeout)
     (gascity-records-serialize-subtables "overrides" overrides gascity-order-override-configuration->toml)
     (gascity-records-serialize-list-of-strings "skip" skip)
     '())))

;; PackCommandEntry — from `$defs.PackCommandEntry'.

(define-record-type* <gascity-pack-command-entry-configuration>
  gascity-pack-command-entry-configuration
  make-gascity-pack-command-entry-configuration
  gascity-pack-command-entry-configuration?
  (description
   gascity-pack-command-entry-configuration-description
   (default 'unset))
  (long-description
   gascity-pack-command-entry-configuration-long-description
   (default 'unset))
  (name
   gascity-pack-command-entry-configuration-name
   (default 'unset))
  (script
   gascity-pack-command-entry-configuration-script
   (default 'unset))
  )

(define (gascity-pack-command-entry-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-pack-command-entry-configuration>."
  (match-record config <gascity-pack-command-entry-configuration>
    (description long-description name script)
    (append
     (gascity-records-serialize-maybe-string "description" description)
     (gascity-records-serialize-maybe-string "long_description" long-description)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-string-or-gexp "script" script)
     '())))

;; PackConfig — from `$defs.PackConfig'.

(define-record-type* <gascity-pack-configuration>
  gascity-pack-configuration
  make-gascity-pack-configuration
  gascity-pack-configuration?
  (agent
   gascity-pack-configuration-agent
   (default 'unset))
  (agent-defaults
   gascity-pack-configuration-agent-defaults
   (default 'unset))
  (commands
   gascity-pack-configuration-commands
   (default 'unset))
  (doctor
   gascity-pack-configuration-doctor
   (default 'unset))
  (global
   gascity-pack-configuration-global
   (default 'unset))
  (imports
   gascity-pack-configuration-imports
   (default 'unset))
  (named-session
   gascity-pack-configuration-named-session
   (default 'unset))
  (pack
   gascity-pack-configuration-pack
   (default 'unset))
  (patches
   gascity-pack-configuration-patches
   (default 'unset))
  (pricing
   gascity-pack-configuration-pricing
   (default 'unset))
  (providers
   gascity-pack-configuration-providers
   (default 'unset))
  (runtimes
   gascity-pack-configuration-runtimes
   (default 'unset))
  (service
   gascity-pack-configuration-service
   (default 'unset))
  (upstreams
   gascity-pack-configuration-upstreams
   (default 'unset))
  (webhook
   gascity-pack-configuration-webhook
   (default 'unset))
  )

(define (gascity-pack-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-pack-configuration>."
  (match-record config <gascity-pack-configuration>
    (agent agent-defaults commands doctor global imports named-session pack patches pricing providers runtimes service upstreams webhook)
    (append
     (gascity-records-serialize-subtables "agent" agent gascity-agent-configuration->toml)
     (gascity-records-serialize-subtable "agent_defaults" agent-defaults gascity-agent-defaults-configuration->toml)
     (gascity-records-serialize-subtables "commands" commands gascity-pack-command-entry-configuration->toml)
     (gascity-records-serialize-subtables "doctor" doctor gascity-pack-doctor-entry-configuration->toml)
     (gascity-records-serialize-subtable "global" global gascity-pack-global-configuration->toml)
     (gascity-records-serialize-alist-subtables "imports" imports gascity-import-configuration->toml)
     (gascity-records-serialize-subtables "named_session" named-session gascity-named-session-configuration->toml)
     (gascity-records-serialize-subtable "pack" pack gascity-pack-meta-configuration->toml)
     (gascity-records-serialize-subtable "patches" patches gascity-pack-patches-configuration->toml)
     (gascity-records-serialize-subtables "pricing" pricing gascity-model-pricing-configuration->toml)
     (gascity-records-serialize-alist-subtables "providers" providers gascity-provider-spec-configuration->toml)
     (gascity-records-serialize-alist-subtables "runtimes" runtimes gascity-pack-runtime-entry-configuration->toml)
     (gascity-records-serialize-subtables "service" service gascity-service-configuration->toml)
     (gascity-records-serialize-alist-subtables "upstreams" upstreams gascity-upstream-spec-configuration->toml)
     (gascity-records-serialize-subtables "webhook" webhook gascity-webhook-configuration->toml)
     '())))

;; PackDefaults — from `$defs.PackDefaults'.

(define-record-type* <gascity-pack-defaults-configuration>
  gascity-pack-defaults-configuration
  make-gascity-pack-defaults-configuration
  gascity-pack-defaults-configuration?
  (rig
   gascity-pack-defaults-configuration-rig
   (default 'unset))
  )

(define (gascity-pack-defaults-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-pack-defaults-configuration>."
  (match-record config <gascity-pack-defaults-configuration>
    (rig)
    (append
     (gascity-records-serialize-subtable "rig" rig gascity-pack-rig-defaults-configuration->toml)
     '())))

;; PackDoctorEntry — from `$defs.PackDoctorEntry'.

(define-record-type* <gascity-pack-doctor-entry-configuration>
  gascity-pack-doctor-entry-configuration
  make-gascity-pack-doctor-entry-configuration
  gascity-pack-doctor-entry-configuration?
  (description
   gascity-pack-doctor-entry-configuration-description
   (default 'unset))
  (fix
   gascity-pack-doctor-entry-configuration-fix
   (default 'unset))
  (name
   gascity-pack-doctor-entry-configuration-name
   (default 'unset))
  (script
   gascity-pack-doctor-entry-configuration-script
   (default 'unset))
  (warmup
   gascity-pack-doctor-entry-configuration-warmup
   (default 'unset))
  )

(define (gascity-pack-doctor-entry-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-pack-doctor-entry-configuration>."
  (match-record config <gascity-pack-doctor-entry-configuration>
    (description fix name script warmup)
    (append
     (gascity-records-serialize-maybe-string "description" description)
     (gascity-records-serialize-string-or-gexp "fix" fix)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-string-or-gexp "script" script)
     (gascity-records-serialize-tri-state "warmup" warmup)
     '())))

;; PackGlobal — from `$defs.PackGlobal'.

(define-record-type* <gascity-pack-global-configuration>
  gascity-pack-global-configuration
  make-gascity-pack-global-configuration
  gascity-pack-global-configuration?
  (session-live
   gascity-pack-global-configuration-session-live
   (default 'unset))
  )

(define (gascity-pack-global-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-pack-global-configuration>."
  (match-record config <gascity-pack-global-configuration>
    (session-live)
    (append
     (gascity-records-serialize-list-of-strings "session_live" session-live)
     '())))

;; PackMeta — from `$defs.PackMeta'.

(define-record-type* <gascity-pack-meta-configuration>
  gascity-pack-meta-configuration
  make-gascity-pack-meta-configuration
  gascity-pack-meta-configuration?
  (description
   gascity-pack-meta-configuration-description
   (default 'unset))
  (includes
   gascity-pack-meta-configuration-includes
   (default 'unset))
  (name
   gascity-pack-meta-configuration-name
   (default 'unset))
  (requires
   gascity-pack-meta-configuration-requires
   (default 'unset))
  (requires-gc
   gascity-pack-meta-configuration-requires-gc
   (default 'unset))
  (schema
   gascity-pack-meta-configuration-schema
   (default 'unset))
  (version
   gascity-pack-meta-configuration-version
   (default 'unset))
  )

(define (gascity-pack-meta-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-pack-meta-configuration>."
  (match-record config <gascity-pack-meta-configuration>
    (description includes name requires requires-gc schema version)
    (append
     (gascity-records-serialize-maybe-string "description" description)
     (gascity-records-serialize-list-of-strings "includes" includes)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-subtables "requires" requires gascity-pack-requirement-configuration->toml)
     (gascity-records-serialize-maybe-string "requires_gc" requires-gc)
     (gascity-records-serialize-maybe-integer "schema" schema)
     (gascity-records-serialize-maybe-string "version" version)
     '())))

;; PackPatches — from `$defs.PackPatches'.

(define-record-type* <gascity-pack-patches-configuration>
  gascity-pack-patches-configuration
  make-gascity-pack-patches-configuration
  gascity-pack-patches-configuration?
  (agent
   gascity-pack-patches-configuration-agent
   (default 'unset))
  )

(define (gascity-pack-patches-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-pack-patches-configuration>."
  (match-record config <gascity-pack-patches-configuration>
    (agent)
    (append
     (gascity-records-serialize-subtables "agent" agent gascity-agent-patch-configuration->toml)
     '())))

;; PackRequirement — from `$defs.PackRequirement'.

(define-record-type* <gascity-pack-requirement-configuration>
  gascity-pack-requirement-configuration
  make-gascity-pack-requirement-configuration
  gascity-pack-requirement-configuration?
  (agent
   gascity-pack-requirement-configuration-agent
   (default 'unset))
  (scope
   gascity-pack-requirement-configuration-scope
   (default 'unset))
  )

(define (gascity-pack-requirement-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-pack-requirement-configuration>."
  (match-record config <gascity-pack-requirement-configuration>
    (agent scope)
    (append
     (gascity-records-serialize-maybe-string "agent" agent)
     (gascity-records-serialize-maybe-string "scope" scope)
     '())))

;; PackRigDefaults — from `$defs.PackRigDefaults'.

(define-record-type* <gascity-pack-rig-defaults-configuration>
  gascity-pack-rig-defaults-configuration
  make-gascity-pack-rig-defaults-configuration
  gascity-pack-rig-defaults-configuration?
  (imports
   gascity-pack-rig-defaults-configuration-imports
   (default 'unset))
  )

(define (gascity-pack-rig-defaults-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-pack-rig-defaults-configuration>."
  (match-record config <gascity-pack-rig-defaults-configuration>
    (imports)
    (append
     (gascity-records-serialize-alist-subtables "imports" imports gascity-import-configuration->toml)
     '())))

;; PackRuntimeEntry — from `$defs.PackRuntimeEntry'.

(define-record-type* <gascity-pack-runtime-entry-configuration>
  gascity-pack-runtime-entry-configuration
  make-gascity-pack-runtime-entry-configuration
  gascity-pack-runtime-entry-configuration?
  (command
   gascity-pack-runtime-entry-configuration-command
   (default 'unset))
  (prompt-delivery
   gascity-pack-runtime-entry-configuration-prompt-delivery
   (default 'unset))
  (protocol
   gascity-pack-runtime-entry-configuration-protocol
   (default 'unset))
  )

(define (gascity-pack-runtime-entry-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-pack-runtime-entry-configuration>."
  (match-record config <gascity-pack-runtime-entry-configuration>
    (command prompt-delivery protocol)
    (append
     (gascity-records-serialize-string-or-gexp "command" command)
     (gascity-records-serialize-maybe-string "prompt_delivery" prompt-delivery)
     (gascity-records-serialize-maybe-integer "protocol" protocol)
     '())))

;; Patches — from `$defs.Patches'.

(define-record-type* <gascity-patches-configuration>
  gascity-patches-configuration
  make-gascity-patches-configuration
  gascity-patches-configuration?
  (agent
   gascity-patches-configuration-agent
   (default 'unset))
  (github-pr-monitor
   gascity-patches-configuration-github-pr-monitor
   (default 'unset))
  (named-session
   gascity-patches-configuration-named-session
   (default 'unset))
  (providers
   gascity-patches-configuration-providers
   (default 'unset))
  (rigs
   gascity-patches-configuration-rigs
   (default 'unset))
  )

(define (gascity-patches-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-patches-configuration>."
  (match-record config <gascity-patches-configuration>
    (agent github-pr-monitor named-session providers rigs)
    (append
     (gascity-records-serialize-subtables "agent" agent gascity-agent-patch-configuration->toml)
     (gascity-records-serialize-subtables "github_pr_monitor" github-pr-monitor gascity-github-pr-monitor-patch-configuration->toml)
     (gascity-records-serialize-subtables "named_session" named-session gascity-named-session-patch-configuration->toml)
     (gascity-records-serialize-subtables "providers" providers gascity-provider-patch-configuration->toml)
     (gascity-records-serialize-subtables "rigs" rigs gascity-rig-patch-configuration->toml)
     '())))

;; PoolOverride — from `$defs.PoolOverride'.

(define-record-type* <gascity-pool-override-configuration>
  gascity-pool-override-configuration
  make-gascity-pool-override-configuration
  gascity-pool-override-configuration?
  (check
   gascity-pool-override-configuration-check
   (default 'unset))
  (drain-timeout
   gascity-pool-override-configuration-drain-timeout
   (default 'unset))
  (max
   gascity-pool-override-configuration-max
   (default 'unset))
  (min
   gascity-pool-override-configuration-min
   (default 'unset))
  (on-boot
   gascity-pool-override-configuration-on-boot
   (default 'unset))
  (on-death
   gascity-pool-override-configuration-on-death
   (default 'unset))
  )

(define (gascity-pool-override-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-pool-override-configuration>."
  (match-record config <gascity-pool-override-configuration>
    (check drain-timeout max min on-boot on-death)
    (append
     (gascity-records-serialize-string-or-gexp "check" check)
     (gascity-records-serialize-maybe-string "drain_timeout" drain-timeout)
     (gascity-records-serialize-maybe-integer "max" max)
     (gascity-records-serialize-maybe-integer "min" min)
     (gascity-records-serialize-string-or-gexp "on_boot" on-boot)
     (gascity-records-serialize-string-or-gexp "on_death" on-death)
     '())))

;; ProviderOption — from `$defs.ProviderOption'.

(define-record-type* <gascity-provider-option-configuration>
  gascity-provider-option-configuration
  make-gascity-provider-option-configuration
  gascity-provider-option-configuration?
  (choices
   gascity-provider-option-configuration-choices
   (default 'unset))
  (default
   gascity-provider-option-configuration-default
   (default 'unset))
  (flag-template
   gascity-provider-option-configuration-flag-template
   (default 'unset))
  (key
   gascity-provider-option-configuration-key
   (default 'unset))
  (label
   gascity-provider-option-configuration-label
   (default 'unset))
  (omit
   gascity-provider-option-configuration-omit
   (default 'unset))
  (type
   gascity-provider-option-configuration-type
   (default 'unset))
  )

(define (gascity-provider-option-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-provider-option-configuration>."
  (match-record config <gascity-provider-option-configuration>
    (choices default flag-template key label omit type)
    (append
     (gascity-records-serialize-subtables "choices" choices gascity-option-choice-configuration->toml)
     (gascity-records-serialize-maybe-string "default" default)
     (gascity-records-serialize-list-of-strings "flag_template" flag-template)
     (gascity-records-serialize-maybe-string "key" key)
     (gascity-records-serialize-maybe-string "label" label)
     (gascity-records-serialize-tri-state "omit" omit)
     (gascity-records-serialize-maybe-string "type" type)
     '())))

;; ProviderPatch — from `$defs.ProviderPatch'.

(define-record-type* <gascity-provider-patch-configuration>
  gascity-provider-patch-configuration
  make-gascity-provider-patch-configuration
  gascity-provider-patch-configuration?
  (-replace
   gascity-provider-patch-configuration--replace
   (default 'unset))
  (accept-startup-dialogs
   gascity-provider-patch-configuration-accept-startup-dialogs
   (default 'unset))
  (acp-args
   gascity-provider-patch-configuration-acp-args
   (default 'unset))
  (acp-command
   gascity-provider-patch-configuration-acp-command
   (default 'unset))
  (args
   gascity-provider-patch-configuration-args
   (default 'unset))
  (args-append
   gascity-provider-patch-configuration-args-append
   (default 'unset))
  (base
   gascity-provider-patch-configuration-base
   (default 'unset))
  (command
   gascity-provider-patch-configuration-command
   (default 'unset))
  (env
   gascity-provider-patch-configuration-env
   (default 'unset))
  (env-remove
   gascity-provider-patch-configuration-env-remove
   (default 'unset))
  (name
   gascity-provider-patch-configuration-name
   (default 'unset))
  (options-schema-merge
   gascity-provider-patch-configuration-options-schema-merge
   (default 'unset))
  (prompt-flag
   gascity-provider-patch-configuration-prompt-flag
   (default 'unset))
  (prompt-mode
   gascity-provider-patch-configuration-prompt-mode
   (default 'unset))
  (ready-delay-ms
   gascity-provider-patch-configuration-ready-delay-ms
   (default 'unset))
  )

(define (gascity-provider-patch-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-provider-patch-configuration>."
  (match-record config <gascity-provider-patch-configuration>
    (-replace accept-startup-dialogs acp-args acp-command args args-append base command env env-remove name options-schema-merge prompt-flag prompt-mode ready-delay-ms)
    (append
     (gascity-records-serialize-tri-state "_replace" -replace)
     (gascity-records-serialize-tri-state "accept_startup_dialogs" accept-startup-dialogs)
     (gascity-records-serialize-list-of-strings "acp_args" acp-args)
     (gascity-records-serialize-string-or-gexp "acp_command" acp-command)
     (gascity-records-serialize-list-of-strings "args" args)
     (gascity-records-serialize-list-of-strings "args_append" args-append)
     (gascity-records-serialize-maybe-string "base" base)
     (gascity-records-serialize-string-or-gexp "command" command)
     (gascity-records-serialize-string-map "env" env)
     (gascity-records-serialize-list-of-strings "env_remove" env-remove)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-maybe-string "options_schema_merge" options-schema-merge)
     (gascity-records-serialize-maybe-string "prompt_flag" prompt-flag)
     (gascity-records-serialize-maybe-string "prompt_mode" prompt-mode)
     (gascity-records-serialize-maybe-integer "ready_delay_ms" ready-delay-ms)
     '())))

;; ProviderSpec — from `$defs.ProviderSpec'.

(define-record-type* <gascity-provider-spec-configuration>
  gascity-provider-spec-configuration
  make-gascity-provider-spec-configuration
  gascity-provider-spec-configuration?
  (accept-startup-dialogs
   gascity-provider-spec-configuration-accept-startup-dialogs
   (default 'unset))
  (acp-args
   gascity-provider-spec-configuration-acp-args
   (default 'unset))
  (acp-command
   gascity-provider-spec-configuration-acp-command
   (default 'unset))
  (args
   gascity-provider-spec-configuration-args
   (default 'unset))
  (args-append
   gascity-provider-spec-configuration-args-append
   (default 'unset))
  (base
   gascity-provider-spec-configuration-base
   (default 'unset))
  (command
   gascity-provider-spec-configuration-command
   (default 'unset))
  (display-name
   gascity-provider-spec-configuration-display-name
   (default 'unset))
  (emits-permission-warning
   gascity-provider-spec-configuration-emits-permission-warning
   (default 'unset))
  (env
   gascity-provider-spec-configuration-env
   (default 'unset))
  (fork-flag
   gascity-provider-spec-configuration-fork-flag
   (default 'unset))
  (instructions-file
   gascity-provider-spec-configuration-instructions-file
   (default 'unset))
  (option-defaults
   gascity-provider-spec-configuration-option-defaults
   (default 'unset))
  (options-schema
   gascity-provider-spec-configuration-options-schema
   (default 'unset))
  (options-schema-merge
   gascity-provider-spec-configuration-options-schema-merge
   (default 'unset))
  (path-check
   gascity-provider-spec-configuration-path-check
   (default 'unset))
  (permission-modes
   gascity-provider-spec-configuration-permission-modes
   (default 'unset))
  (print-args
   gascity-provider-spec-configuration-print-args
   (default 'unset))
  (process-names
   gascity-provider-spec-configuration-process-names
   (default 'unset))
  (prompt-flag
   gascity-provider-spec-configuration-prompt-flag
   (default 'unset))
  (prompt-mode
   gascity-provider-spec-configuration-prompt-mode
   (default 'unset))
  (ready-delay-ms
   gascity-provider-spec-configuration-ready-delay-ms
   (default 'unset))
  (ready-prompt-prefix
   gascity-provider-spec-configuration-ready-prompt-prefix
   (default 'unset))
  (resume-command
   gascity-provider-spec-configuration-resume-command
   (default 'unset))
  (resume-flag
   gascity-provider-spec-configuration-resume-flag
   (default 'unset))
  (resume-style
   gascity-provider-spec-configuration-resume-style
   (default 'unset))
  (session-id-flag
   gascity-provider-spec-configuration-session-id-flag
   (default 'unset))
  (supports-acp
   gascity-provider-spec-configuration-supports-acp
   (default 'unset))
  (supports-hooks
   gascity-provider-spec-configuration-supports-hooks
   (default 'unset))
  (title-model
   gascity-provider-spec-configuration-title-model
   (default 'unset))
  (upstream-env
   gascity-provider-spec-configuration-upstream-env
   (default 'unset))
  )

(define (gascity-provider-spec-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-provider-spec-configuration>."
  (match-record config <gascity-provider-spec-configuration>
    (accept-startup-dialogs acp-args acp-command args args-append base command display-name emits-permission-warning env fork-flag instructions-file option-defaults options-schema options-schema-merge path-check permission-modes print-args process-names prompt-flag prompt-mode ready-delay-ms ready-prompt-prefix resume-command resume-flag resume-style session-id-flag supports-acp supports-hooks title-model upstream-env)
    (append
     (gascity-records-serialize-tri-state "accept_startup_dialogs" accept-startup-dialogs)
     (gascity-records-serialize-list-of-strings "acp_args" acp-args)
     (gascity-records-serialize-string-or-gexp "acp_command" acp-command)
     (gascity-records-serialize-list-of-strings "args" args)
     (gascity-records-serialize-list-of-strings "args_append" args-append)
     (gascity-records-serialize-maybe-string "base" base)
     (gascity-records-serialize-string-or-gexp "command" command)
     (gascity-records-serialize-maybe-string "display_name" display-name)
     (gascity-records-serialize-tri-state "emits_permission_warning" emits-permission-warning)
     (gascity-records-serialize-string-map "env" env)
     (gascity-records-serialize-maybe-string "fork_flag" fork-flag)
     (gascity-records-serialize-maybe-string "instructions_file" instructions-file)
     (gascity-records-serialize-string-map "option_defaults" option-defaults)
     (gascity-records-serialize-subtables "options_schema" options-schema gascity-provider-option-configuration->toml)
     (gascity-records-serialize-maybe-string "options_schema_merge" options-schema-merge)
     (gascity-records-serialize-maybe-string "path_check" path-check)
     (gascity-records-serialize-string-map "permission_modes" permission-modes)
     (gascity-records-serialize-list-of-strings "print_args" print-args)
     (gascity-records-serialize-list-of-strings "process_names" process-names)
     (gascity-records-serialize-maybe-string "prompt_flag" prompt-flag)
     (gascity-records-serialize-maybe-string "prompt_mode" prompt-mode)
     (gascity-records-serialize-maybe-integer "ready_delay_ms" ready-delay-ms)
     (gascity-records-serialize-maybe-string "ready_prompt_prefix" ready-prompt-prefix)
     (gascity-records-serialize-string-or-gexp "resume_command" resume-command)
     (gascity-records-serialize-maybe-string "resume_flag" resume-flag)
     (gascity-records-serialize-maybe-string "resume_style" resume-style)
     (gascity-records-serialize-maybe-string "session_id_flag" session-id-flag)
     (gascity-records-serialize-tri-state "supports_acp" supports-acp)
     (gascity-records-serialize-tri-state "supports_hooks" supports-hooks)
     (gascity-records-serialize-maybe-string "title_model" title-model)
     (gascity-records-serialize-subtable "upstream_env" upstream-env gascity-upstream-env-binding-configuration->toml)
     '())))

;; Rig — from `$defs.Rig'.

(define-record-type* <gascity-rig-configuration>
  gascity-rig-configuration
  make-gascity-rig-configuration
  gascity-rig-configuration?
  (default-branch
   gascity-rig-configuration-default-branch
   (default 'unset))
  (default-sling-target
   gascity-rig-configuration-default-sling-target
   (default 'unset))
  (default-sling-targets
   gascity-rig-configuration-default-sling-targets
   (default 'unset))
  (dolt-host
   gascity-rig-configuration-dolt-host
   (default 'unset))
  (dolt-port
   gascity-rig-configuration-dolt-port
   (default 'unset))
  (formula-vars
   gascity-rig-configuration-formula-vars
   (default 'unset))
  (formulas-dir
   gascity-rig-configuration-formulas-dir
   (default 'unset))
  (imports
   gascity-rig-configuration-imports
   (default 'unset))
  (includes
   gascity-rig-configuration-includes
   (default 'unset))
  (max-active-sessions
   gascity-rig-configuration-max-active-sessions
   (default 'unset))
  (name
   gascity-rig-configuration-name
   (default 'unset))
  (overrides
   gascity-rig-configuration-overrides
   (default 'unset))
  (patches
   gascity-rig-configuration-patches
   (default 'unset))
  (path
   gascity-rig-configuration-path
   (default 'unset))
  (prefix
   gascity-rig-configuration-prefix
   (default 'unset))
  (session-sleep
   gascity-rig-configuration-session-sleep
   (default 'unset))
  (suspended
   gascity-rig-configuration-suspended
   (default 'unset))
  (suspended-on-start
   gascity-rig-configuration-suspended-on-start
   (default 'unset))
  )

(define (gascity-rig-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-rig-configuration>."
  (match-record config <gascity-rig-configuration>
    (default-branch default-sling-target default-sling-targets dolt-host dolt-port formula-vars formulas-dir imports includes max-active-sessions name overrides patches path prefix session-sleep suspended suspended-on-start)
    (append
     (gascity-records-serialize-maybe-string "default_branch" default-branch)
     (gascity-records-serialize-maybe-string "default_sling_target" default-sling-target)
     (gascity-records-serialize-list-of-strings "default_sling_targets" default-sling-targets)
     (gascity-records-serialize-maybe-string "dolt_host" dolt-host)
     (gascity-records-serialize-maybe-string "dolt_port" dolt-port)
     (gascity-records-serialize-string-map "formula_vars" formula-vars)
     (gascity-records-serialize-maybe-string "formulas_dir" formulas-dir)
     (gascity-records-serialize-alist-subtables "imports" imports gascity-import-configuration->toml)
     (gascity-records-serialize-list-of-strings "includes" includes)
     (gascity-records-serialize-maybe-integer "max_active_sessions" max-active-sessions)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-subtables "overrides" overrides gascity-agent-override-configuration->toml)
     (gascity-records-serialize-subtables "patches" patches gascity-agent-override-configuration->toml)
     (gascity-records-serialize-maybe-string "path" path)
     (gascity-records-serialize-maybe-string "prefix" prefix)
     (gascity-records-serialize-subtable "session_sleep" session-sleep gascity-session-sleep-configuration->toml)
     (gascity-records-serialize-tri-state "suspended" suspended)
     (gascity-records-serialize-tri-state "suspended_on_start" suspended-on-start)
     '())))

;; RigPatch — from `$defs.RigPatch'.

(define-record-type* <gascity-rig-patch-configuration>
  gascity-rig-patch-configuration
  make-gascity-rig-patch-configuration
  gascity-rig-patch-configuration?
  (default-branch
   gascity-rig-patch-configuration-default-branch
   (default 'unset))
  (formula-vars
   gascity-rig-patch-configuration-formula-vars
   (default 'unset))
  (name
   gascity-rig-patch-configuration-name
   (default 'unset))
  (path
   gascity-rig-patch-configuration-path
   (default 'unset))
  (prefix
   gascity-rig-patch-configuration-prefix
   (default 'unset))
  (suspended
   gascity-rig-patch-configuration-suspended
   (default 'unset))
  (suspended-on-start
   gascity-rig-patch-configuration-suspended-on-start
   (default 'unset))
  )

(define (gascity-rig-patch-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-rig-patch-configuration>."
  (match-record config <gascity-rig-patch-configuration>
    (default-branch formula-vars name path prefix suspended suspended-on-start)
    (append
     (gascity-records-serialize-maybe-string "default_branch" default-branch)
     (gascity-records-serialize-string-map "formula_vars" formula-vars)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-maybe-string "path" path)
     (gascity-records-serialize-maybe-string "prefix" prefix)
     (gascity-records-serialize-tri-state "suspended" suspended)
     (gascity-records-serialize-tri-state "suspended_on_start" suspended-on-start)
     '())))

;; Service — from `$defs.Service'.

(define-record-type* <gascity-service-configuration>
  gascity-service-configuration
  make-gascity-service-configuration
  gascity-service-configuration?
  (kind
   gascity-service-configuration-kind
   (default 'unset))
  (name
   gascity-service-configuration-name
   (default 'unset))
  (process
   gascity-service-configuration-process
   (default 'unset))
  (publication
   gascity-service-configuration-publication
   (default 'unset))
  (publish-mode
   gascity-service-configuration-publish-mode
   (default 'unset))
  (state-root
   gascity-service-configuration-state-root
   (default 'unset))
  (workflow
   gascity-service-configuration-workflow
   (default 'unset))
  )

(define (gascity-service-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-service-configuration>."
  (match-record config <gascity-service-configuration>
    (kind name process publication publish-mode state-root workflow)
    (append
     (gascity-records-serialize-maybe-string "kind" kind)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-subtable "process" process gascity-service-process-configuration->toml)
     (gascity-records-serialize-subtable "publication" publication gascity-service-publication-configuration->toml)
     (gascity-records-serialize-maybe-string "publish_mode" publish-mode)
     (gascity-records-serialize-maybe-string "state_root" state-root)
     (gascity-records-serialize-subtable "workflow" workflow gascity-service-workflow-configuration->toml)
     '())))

;; ServiceProcessConfig — from `$defs.ServiceProcessConfig'.

(define-record-type* <gascity-service-process-configuration>
  gascity-service-process-configuration
  make-gascity-service-process-configuration
  gascity-service-process-configuration?
  (command
   gascity-service-process-configuration-command
   (default 'unset))
  (health-path
   gascity-service-process-configuration-health-path
   (default 'unset))
  )

(define (gascity-service-process-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-service-process-configuration>."
  (match-record config <gascity-service-process-configuration>
    (command health-path)
    (append
     (gascity-records-serialize-list-of-strings "command" command)
     (gascity-records-serialize-maybe-string "health_path" health-path)
     '())))

;; ServicePublicationConfig — from `$defs.ServicePublicationConfig'.

(define-record-type* <gascity-service-publication-configuration>
  gascity-service-publication-configuration
  make-gascity-service-publication-configuration
  gascity-service-publication-configuration?
  (allow-websockets
   gascity-service-publication-configuration-allow-websockets
   (default 'unset))
  (hostname
   gascity-service-publication-configuration-hostname
   (default 'unset))
  (visibility
   gascity-service-publication-configuration-visibility
   (default 'unset))
  )

(define (gascity-service-publication-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-service-publication-configuration>."
  (match-record config <gascity-service-publication-configuration>
    (allow-websockets hostname visibility)
    (append
     (gascity-records-serialize-tri-state "allow_websockets" allow-websockets)
     (gascity-records-serialize-maybe-string "hostname" hostname)
     (gascity-records-serialize-maybe-string "visibility" visibility)
     '())))

;; ServiceWorkflowConfig — from `$defs.ServiceWorkflowConfig'.

(define-record-type* <gascity-service-workflow-configuration>
  gascity-service-workflow-configuration
  make-gascity-service-workflow-configuration
  gascity-service-workflow-configuration?
  (contract
   gascity-service-workflow-configuration-contract
   (default 'unset))
  )

(define (gascity-service-workflow-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-service-workflow-configuration>."
  (match-record config <gascity-service-workflow-configuration>
    (contract)
    (append
     (gascity-records-serialize-maybe-string "contract" contract)
     '())))

;; SessionConfig — from `$defs.SessionConfig'.

(define-record-type* <gascity-session-configuration>
  gascity-session-configuration
  make-gascity-session-configuration
  gascity-session-configuration?
  (acp
   gascity-session-configuration-acp
   (default 'unset))
  (claim-holder-stall-timeout
   gascity-session-configuration-claim-holder-stall-timeout
   (default 'unset))
  (debounce-ms
   gascity-session-configuration-debounce-ms
   (default 'unset))
  (display-ms
   gascity-session-configuration-display-ms
   (default 'unset))
  (k8s
   gascity-session-configuration-k8s
   (default 'unset))
  (nudge-lock-timeout
   gascity-session-configuration-nudge-lock-timeout
   (default 'unset))
  (nudge-poll-interval
   gascity-session-configuration-nudge-poll-interval
   (default 'unset))
  (nudge-ready-timeout
   gascity-session-configuration-nudge-ready-timeout
   (default 'unset))
  (nudge-retry-interval
   gascity-session-configuration-nudge-retry-interval
   (default 'unset))
  (progress-stall-timeout
   gascity-session-configuration-progress-stall-timeout
   (default 'unset))
  (provider
   gascity-session-configuration-provider
   (default 'unset))
  (remote-match
   gascity-session-configuration-remote-match
   (default 'unset))
  (setup-max-timeout
   gascity-session-configuration-setup-max-timeout
   (default 'unset))
  (setup-timeout
   gascity-session-configuration-setup-timeout
   (default 'unset))
  (socket
   gascity-session-configuration-socket
   (default 'unset))
  (startup-timeout
   gascity-session-configuration-startup-timeout
   (default 'unset))
  )

(define (gascity-session-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-session-configuration>."
  (match-record config <gascity-session-configuration>
    (acp claim-holder-stall-timeout debounce-ms display-ms k8s nudge-lock-timeout nudge-poll-interval nudge-ready-timeout nudge-retry-interval progress-stall-timeout provider remote-match setup-max-timeout setup-timeout socket startup-timeout)
    (append
     (gascity-records-serialize-subtable "acp" acp gascity-acp-session-configuration->toml)
     (gascity-records-serialize-maybe-string "claim_holder_stall_timeout" claim-holder-stall-timeout)
     (gascity-records-serialize-maybe-integer "debounce_ms" debounce-ms)
     (gascity-records-serialize-maybe-integer "display_ms" display-ms)
     (gascity-records-serialize-subtable "k8s" k8s gascity-k8s-configuration->toml)
     (gascity-records-serialize-maybe-string "nudge_lock_timeout" nudge-lock-timeout)
     (gascity-records-serialize-maybe-string "nudge_poll_interval" nudge-poll-interval)
     (gascity-records-serialize-maybe-string "nudge_ready_timeout" nudge-ready-timeout)
     (gascity-records-serialize-maybe-string "nudge_retry_interval" nudge-retry-interval)
     (gascity-records-serialize-maybe-string "progress_stall_timeout" progress-stall-timeout)
     (gascity-records-serialize-maybe-string "provider" provider)
     (gascity-records-serialize-maybe-string "remote_match" remote-match)
     (gascity-records-serialize-maybe-string "setup_max_timeout" setup-max-timeout)
     (gascity-records-serialize-maybe-string "setup_timeout" setup-timeout)
     (gascity-records-serialize-maybe-string "socket" socket)
     (gascity-records-serialize-maybe-string "startup_timeout" startup-timeout)
     '())))

;; SessionSleepConfig — from `$defs.SessionSleepConfig'.

(define-record-type* <gascity-session-sleep-configuration>
  gascity-session-sleep-configuration
  make-gascity-session-sleep-configuration
  gascity-session-sleep-configuration?
  (interactive-fresh
   gascity-session-sleep-configuration-interactive-fresh
   (default 'unset))
  (interactive-resume
   gascity-session-sleep-configuration-interactive-resume
   (default 'unset))
  (noninteractive
   gascity-session-sleep-configuration-noninteractive
   (default 'unset))
  )

(define (gascity-session-sleep-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-session-sleep-configuration>."
  (match-record config <gascity-session-sleep-configuration>
    (interactive-fresh interactive-resume noninteractive)
    (append
     (gascity-records-serialize-maybe-string "interactive_fresh" interactive-fresh)
     (gascity-records-serialize-maybe-string "interactive_resume" interactive-resume)
     (gascity-records-serialize-maybe-string "noninteractive" noninteractive)
     '())))

;; StorageBindingConfig — from `$defs.StorageBindingConfig'.

(define-record-type* <gascity-storage-binding-configuration>
  gascity-storage-binding-configuration
  make-gascity-storage-binding-configuration
  gascity-storage-binding-configuration?
  (auth
   gascity-storage-binding-configuration-auth
   (default 'unset))
  (config-ref
   gascity-storage-binding-configuration-config-ref
   (default 'unset))
  (path
   gascity-storage-binding-configuration-path
   (default 'unset))
  (provider
   gascity-storage-binding-configuration-provider
   (default 'unset))
  (url
   gascity-storage-binding-configuration-url
   (default 'unset))
  )

(define (gascity-storage-binding-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-storage-binding-configuration>."
  (match-record config <gascity-storage-binding-configuration>
    (auth config-ref path provider url)
    (append
     (gascity-records-serialize-maybe-string "auth" auth)
     (gascity-records-serialize-maybe-string "config_ref" config-ref)
     (gascity-records-serialize-maybe-string "path" path)
     (gascity-records-serialize-maybe-string "provider" provider)
     (gascity-records-serialize-maybe-string "url" url)
     '())))

;; StorageClasses — from `$defs.StorageClasses'.

(define-record-type* <gascity-storage-classes-configuration>
  gascity-storage-classes-configuration
  make-gascity-storage-classes-configuration
  gascity-storage-classes-configuration?
  (graph
   gascity-storage-classes-configuration-graph
   (default 'unset))
  (messaging
   gascity-storage-classes-configuration-messaging
   (default 'unset))
  (nudges
   gascity-storage-classes-configuration-nudges
   (default 'unset))
  (orders
   gascity-storage-classes-configuration-orders
   (default 'unset))
  (sessions
   gascity-storage-classes-configuration-sessions
   (default 'unset))
  (work
   gascity-storage-classes-configuration-work
   (default 'unset))
  )

(define (gascity-storage-classes-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-storage-classes-configuration>."
  (match-record config <gascity-storage-classes-configuration>
    (graph messaging nudges orders sessions work)
    (append
     (gascity-records-serialize-maybe-string "graph" graph)
     (gascity-records-serialize-maybe-string "messaging" messaging)
     (gascity-records-serialize-maybe-string "nudges" nudges)
     (gascity-records-serialize-maybe-string "orders" orders)
     (gascity-records-serialize-maybe-string "sessions" sessions)
     (gascity-records-serialize-maybe-string "work" work)
     '())))

;; StorageConfig — from `$defs.StorageConfig'.

(define-record-type* <gascity-storage-configuration>
  gascity-storage-configuration
  make-gascity-storage-configuration
  gascity-storage-configuration?
  (bindings
   gascity-storage-configuration-bindings
   (default 'unset))
  (classes
   gascity-storage-configuration-classes
   (default 'unset))
  )

(define (gascity-storage-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-storage-configuration>."
  (match-record config <gascity-storage-configuration>
    (bindings classes)
    (append
     (gascity-records-serialize-alist-subtables "bindings" bindings gascity-storage-binding-configuration->toml)
     (gascity-records-serialize-subtable "classes" classes gascity-storage-classes-configuration->toml)
     '())))

;; Tier — from `$defs.Tier'.

(define-record-type* <gascity-tier-configuration>
  gascity-tier-configuration
  make-gascity-tier-configuration
  gascity-tier-configuration?
  (cache-creation-usd-per-1m
   gascity-tier-configuration-cache-creation-usd-per-1m
   (default 'unset))
  (cache-read-usd-per-1m
   gascity-tier-configuration-cache-read-usd-per-1m
   (default 'unset))
  (completion-usd-per-1m
   gascity-tier-configuration-completion-usd-per-1m
   (default 'unset))
  (prompt-usd-per-1m
   gascity-tier-configuration-prompt-usd-per-1m
   (default 'unset))
  )

(define (gascity-tier-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-tier-configuration>."
  (match-record config <gascity-tier-configuration>
    (cache-creation-usd-per-1m cache-read-usd-per-1m completion-usd-per-1m prompt-usd-per-1m)
    (append
     (gascity-records-serialize-maybe-real "cache_creation_usd_per_1m" cache-creation-usd-per-1m)
     (gascity-records-serialize-maybe-real "cache_read_usd_per_1m" cache-read-usd-per-1m)
     (gascity-records-serialize-maybe-real "completion_usd_per_1m" completion-usd-per-1m)
     (gascity-records-serialize-maybe-real "prompt_usd_per_1m" prompt-usd-per-1m)
     '())))

;; UpstreamEnvBinding — from `$defs.UpstreamEnvBinding'.

(define-record-type* <gascity-upstream-env-binding-configuration>
  gascity-upstream-env-binding-configuration
  make-gascity-upstream-env-binding-configuration
  gascity-upstream-env-binding-configuration?
  (api-key
   gascity-upstream-env-binding-configuration-api-key
   (default 'unset))
  (auth-token
   gascity-upstream-env-binding-configuration-auth-token
   (default 'unset))
  (base-url
   gascity-upstream-env-binding-configuration-base-url
   (default 'unset))
  )

(define (gascity-upstream-env-binding-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-upstream-env-binding-configuration>."
  (match-record config <gascity-upstream-env-binding-configuration>
    (api-key auth-token base-url)
    (append
     (gascity-records-serialize-secret "api_key" api-key)
     (gascity-records-serialize-secret "auth_token" auth-token)
     (gascity-records-serialize-maybe-string "base_url" base-url)
     '())))

;; UpstreamSpec — from `$defs.UpstreamSpec'.

(define-record-type* <gascity-upstream-spec-configuration>
  gascity-upstream-spec-configuration
  make-gascity-upstream-spec-configuration
  gascity-upstream-spec-configuration?
  (api-key
   gascity-upstream-spec-configuration-api-key
   (default 'unset))
  (api-key-env
   gascity-upstream-spec-configuration-api-key-env
   (default 'unset))
  (auth-token
   gascity-upstream-spec-configuration-auth-token
   (default 'unset))
  (auth-token-env
   gascity-upstream-spec-configuration-auth-token-env
   (default 'unset))
  (base-url
   gascity-upstream-spec-configuration-base-url
   (default 'unset))
  (base-url-env
   gascity-upstream-spec-configuration-base-url-env
   (default 'unset))
  (description
   gascity-upstream-spec-configuration-description
   (default 'unset))
  (env
   gascity-upstream-spec-configuration-env
   (default 'unset))
  )

(define (gascity-upstream-spec-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-upstream-spec-configuration>."
  (match-record config <gascity-upstream-spec-configuration>
    (api-key api-key-env auth-token auth-token-env base-url base-url-env description env)
    (append
     (gascity-records-serialize-secret "api_key" api-key)
     (gascity-records-serialize-maybe-string "api_key_env" api-key-env)
     (gascity-records-serialize-secret "auth_token" auth-token)
     (gascity-records-serialize-maybe-string "auth_token_env" auth-token-env)
     (gascity-records-serialize-maybe-string "base_url" base-url)
     (gascity-records-serialize-maybe-string "base_url_env" base-url-env)
     (gascity-records-serialize-maybe-string "description" description)
     (gascity-records-serialize-string-map "env" env)
     '())))

;; UsageConfig — from `$defs.UsageConfig'.

(define-record-type* <gascity-usage-configuration>
  gascity-usage-configuration
  make-gascity-usage-configuration
  gascity-usage-configuration?
  (provider
   gascity-usage-configuration-provider
   (default 'unset))
  )

(define (gascity-usage-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-usage-configuration>."
  (match-record config <gascity-usage-configuration>
    (provider)
    (append
     (gascity-records-serialize-maybe-string "provider" provider)
     '())))

;; Webhook — from `$defs.Webhook'.

(define-record-type* <gascity-webhook-configuration>
  gascity-webhook-configuration
  make-gascity-webhook-configuration
  gascity-webhook-configuration?
  (max-per-minute
   gascity-webhook-configuration-max-per-minute
   (default 'unset))
  (name
   gascity-webhook-configuration-name
   (default 'unset))
  (publication
   gascity-webhook-configuration-publication
   (default 'unset))
  (rig
   gascity-webhook-configuration-rig
   (default 'unset))
  (rule
   gascity-webhook-configuration-rule
   (default 'unset))
  (scope
   gascity-webhook-configuration-scope
   (default 'unset))
  (verify
   gascity-webhook-configuration-verify
   (default 'unset))
  )

(define (gascity-webhook-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-webhook-configuration>."
  (match-record config <gascity-webhook-configuration>
    (max-per-minute name publication rig rule scope verify)
    (append
     (gascity-records-serialize-maybe-integer "max_per_minute" max-per-minute)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-subtable "publication" publication gascity-service-publication-configuration->toml)
     (gascity-records-serialize-maybe-string "rig" rig)
     (gascity-records-serialize-subtables "rule" rule gascity-webhook-rule-configuration->toml)
     (gascity-records-serialize-maybe-string "scope" scope)
     (gascity-records-serialize-subtable "verify" verify gascity-webhook-verify-configuration->toml)
     '())))

;; WebhookAllowPublic — from `$defs.WebhookAllowPublic'.

(define-record-type* <gascity-webhook-allow-public-configuration>
  gascity-webhook-allow-public-configuration
  make-gascity-webhook-allow-public-configuration
  gascity-webhook-allow-public-configuration?
  (digest
   gascity-webhook-allow-public-configuration-digest
   (default 'unset))
  (name
   gascity-webhook-allow-public-configuration-name
   (default 'unset))
  (source
   gascity-webhook-allow-public-configuration-source
   (default 'unset))
  )

(define (gascity-webhook-allow-public-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-webhook-allow-public-configuration>."
  (match-record config <gascity-webhook-allow-public-configuration>
    (digest name source)
    (append
     (gascity-records-serialize-maybe-string "digest" digest)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-maybe-string "source" source)
     '())))

;; WebhookJWTPolicy — from `$defs.WebhookJWTPolicy'.

(define-record-type* <gascity-webhook-jwt-policy-configuration>
  gascity-webhook-jwt-policy-configuration
  make-gascity-webhook-jwt-policy-configuration
  gascity-webhook-jwt-policy-configuration?
  (audience
   gascity-webhook-jwt-policy-configuration-audience
   (default 'unset))
  (issuer
   gascity-webhook-jwt-policy-configuration-issuer
   (default 'unset))
  (jwks-url
   gascity-webhook-jwt-policy-configuration-jwks-url
   (default 'unset))
  (name
   gascity-webhook-jwt-policy-configuration-name
   (default 'unset))
  )

(define (gascity-webhook-jwt-policy-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-webhook-jwt-policy-configuration>."
  (match-record config <gascity-webhook-jwt-policy-configuration>
    (audience issuer jwks-url name)
    (append
     (gascity-records-serialize-maybe-string "audience" audience)
     (gascity-records-serialize-maybe-string "issuer" issuer)
     (gascity-records-serialize-maybe-string "jwks_url" jwks-url)
     (gascity-records-serialize-maybe-string "name" name)
     '())))

;; WebhookPolicyConfig — from `$defs.WebhookPolicyConfig'.

(define-record-type* <gascity-webhook-policy-configuration>
  gascity-webhook-policy-configuration
  make-gascity-webhook-policy-configuration
  gascity-webhook-policy-configuration?
  (allow-public
   gascity-webhook-policy-configuration-allow-public
   (default 'unset))
  (jwt-policy
   gascity-webhook-policy-configuration-jwt-policy
   (default 'unset))
  (rate-limit
   gascity-webhook-policy-configuration-rate-limit
   (default 'unset))
  )

(define (gascity-webhook-policy-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-webhook-policy-configuration>."
  (match-record config <gascity-webhook-policy-configuration>
    (allow-public jwt-policy rate-limit)
    (append
     (gascity-records-serialize-subtables "allow_public" allow-public gascity-webhook-allow-public-configuration->toml)
     (gascity-records-serialize-subtables "jwt_policy" jwt-policy gascity-webhook-jwt-policy-configuration->toml)
     (gascity-records-serialize-subtable "rate_limit" rate-limit gascity-webhook-rate-limit-configuration->toml)
     '())))

;; WebhookRateLimitConfig — from `$defs.WebhookRateLimitConfig'.

(define-record-type* <gascity-webhook-rate-limit-configuration>
  gascity-webhook-rate-limit-configuration
  make-gascity-webhook-rate-limit-configuration
  gascity-webhook-rate-limit-configuration?
  (burst
   gascity-webhook-rate-limit-configuration-burst
   (default 'unset))
  (override
   gascity-webhook-rate-limit-configuration-override
   (default 'unset))
  (per-minute
   gascity-webhook-rate-limit-configuration-per-minute
   (default 'unset))
  )

(define (gascity-webhook-rate-limit-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-webhook-rate-limit-configuration>."
  (match-record config <gascity-webhook-rate-limit-configuration>
    (burst override per-minute)
    (append
     (gascity-records-serialize-maybe-integer "burst" burst)
     (gascity-records-serialize-subtables "override" override gascity-webhook-rate-limit-override-configuration->toml)
     (gascity-records-serialize-maybe-integer "per_minute" per-minute)
     '())))

;; WebhookRateLimitOverride — from `$defs.WebhookRateLimitOverride'.

(define-record-type* <gascity-webhook-rate-limit-override-configuration>
  gascity-webhook-rate-limit-override-configuration
  make-gascity-webhook-rate-limit-override-configuration
  gascity-webhook-rate-limit-override-configuration?
  (burst
   gascity-webhook-rate-limit-override-configuration-burst
   (default 'unset))
  (name
   gascity-webhook-rate-limit-override-configuration-name
   (default 'unset))
  (per-minute
   gascity-webhook-rate-limit-override-configuration-per-minute
   (default 'unset))
  )

(define (gascity-webhook-rate-limit-override-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-webhook-rate-limit-override-configuration>."
  (match-record config <gascity-webhook-rate-limit-override-configuration>
    (burst name per-minute)
    (append
     (gascity-records-serialize-maybe-integer "burst" burst)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-maybe-integer "per_minute" per-minute)
     '())))

;; WebhookRule — from `$defs.WebhookRule'.

(define-record-type* <gascity-webhook-rule-configuration>
  gascity-webhook-rule-configuration
  make-gascity-webhook-rule-configuration
  gascity-webhook-rule-configuration?
  (args
   gascity-webhook-rule-configuration-args
   (default 'unset))
  (event
   gascity-webhook-rule-configuration-event
   (default 'unset))
  (match
   gascity-webhook-rule-configuration-match
   (default 'unset))
  (order
   gascity-webhook-rule-configuration-order
   (default 'unset))
  (rig
   gascity-webhook-rule-configuration-rig
   (default 'unset))
  (target
   gascity-webhook-rule-configuration-target
   (default 'unset))
  )

(define (gascity-webhook-rule-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-webhook-rule-configuration>."
  (match-record config <gascity-webhook-rule-configuration>
    (args event match order rig target)
    (append
     (gascity-records-serialize-string-map "args" args)
     (gascity-records-serialize-maybe-string "event" event)
     (gascity-records-serialize-string-map "match" match)
     (gascity-records-serialize-maybe-string "order" order)
     (gascity-records-serialize-maybe-string "rig" rig)
     (gascity-records-serialize-maybe-string "target" target)
     '())))

;; WebhookVerify — from `$defs.WebhookVerify'.

(define-record-type* <gascity-webhook-verify-configuration>
  gascity-webhook-verify-configuration
  make-gascity-webhook-verify-configuration
  gascity-webhook-verify-configuration?
  (allowed-cidrs
   gascity-webhook-verify-configuration-allowed-cidrs
   (default 'unset))
  (audience
   gascity-webhook-verify-configuration-audience
   (default 'unset))
  (bearer-env
   gascity-webhook-verify-configuration-bearer-env
   (default 'unset))
  (dedup-header
   gascity-webhook-verify-configuration-dedup-header
   (default 'unset))
  (event-header
   gascity-webhook-verify-configuration-event-header
   (default 'unset))
  (issuer
   gascity-webhook-verify-configuration-issuer
   (default 'unset))
  (jwks-url
   gascity-webhook-verify-configuration-jwks-url
   (default 'unset))
  (replay-window
   gascity-webhook-verify-configuration-replay-window
   (default 'unset))
  (scheme
   gascity-webhook-verify-configuration-scheme
   (default 'unset))
  (secret-env
   gascity-webhook-verify-configuration-secret-env
   (default 'unset))
  (secret-key
   gascity-webhook-verify-configuration-secret-key
   (default 'unset))
  (signature-header
   gascity-webhook-verify-configuration-signature-header
   (default 'unset))
  (timestamp-header
   gascity-webhook-verify-configuration-timestamp-header
   (default 'unset))
  )

(define (gascity-webhook-verify-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-webhook-verify-configuration>."
  (match-record config <gascity-webhook-verify-configuration>
    (allowed-cidrs audience bearer-env dedup-header event-header issuer jwks-url replay-window scheme secret-env secret-key signature-header timestamp-header)
    (append
     (gascity-records-serialize-list-of-strings "allowed_cidrs" allowed-cidrs)
     (gascity-records-serialize-maybe-string "audience" audience)
     (gascity-records-serialize-maybe-string "bearer_env" bearer-env)
     (gascity-records-serialize-maybe-string "dedup_header" dedup-header)
     (gascity-records-serialize-maybe-string "event_header" event-header)
     (gascity-records-serialize-maybe-string "issuer" issuer)
     (gascity-records-serialize-maybe-string "jwks_url" jwks-url)
     (gascity-records-serialize-maybe-string "replay_window" replay-window)
     (gascity-records-serialize-maybe-string "scheme" scheme)
     (gascity-records-serialize-maybe-string "secret_env" secret-env)
     (gascity-records-serialize-maybe-string "secret_key" secret-key)
     (gascity-records-serialize-maybe-string "signature_header" signature-header)
     (gascity-records-serialize-maybe-string "timestamp_header" timestamp-header)
     '())))

;; Workspace — from `$defs.Workspace'.

(define-record-type* <gascity-workspace-configuration>
  gascity-workspace-configuration
  make-gascity-workspace-configuration
  gascity-workspace-configuration?
  (default-rig-includes
   gascity-workspace-configuration-default-rig-includes
   (default 'unset))
  (env
   gascity-workspace-configuration-env
   (default 'unset))
  (global-fragments
   gascity-workspace-configuration-global-fragments
   (default 'unset))
  (includes
   gascity-workspace-configuration-includes
   (default 'unset))
  (install-agent-hooks
   gascity-workspace-configuration-install-agent-hooks
   (default 'unset))
  (max-active-sessions
   gascity-workspace-configuration-max-active-sessions
   (default 'unset))
  (name
   gascity-workspace-configuration-name
   (default 'unset))
  (prefix
   gascity-workspace-configuration-prefix
   (default 'unset))
  (provider
   gascity-workspace-configuration-provider
   (default 'unset))
  (session-template
   gascity-workspace-configuration-session-template
   (default 'unset))
  (start-command
   gascity-workspace-configuration-start-command
   (default 'unset))
  (suspended
   gascity-workspace-configuration-suspended
   (default 'unset))
  (suspended-on-start
   gascity-workspace-configuration-suspended-on-start
   (default 'unset))
  (timezone
   gascity-workspace-configuration-timezone
   (default 'unset))
  )

(define (gascity-workspace-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a <gascity-workspace-configuration>."
  (match-record config <gascity-workspace-configuration>
    (default-rig-includes env global-fragments includes install-agent-hooks max-active-sessions name prefix provider session-template start-command suspended suspended-on-start timezone)
    (append
     (gascity-records-serialize-list-of-strings "default_rig_includes" default-rig-includes)
     (gascity-records-serialize-string-map "env" env)
     (gascity-records-serialize-list-of-strings "global_fragments" global-fragments)
     (gascity-records-serialize-list-of-strings "includes" includes)
     (gascity-records-serialize-list-of-strings "install_agent_hooks" install-agent-hooks)
     (gascity-records-serialize-maybe-integer "max_active_sessions" max-active-sessions)
     (gascity-records-serialize-maybe-string "name" name)
     (gascity-records-serialize-maybe-string "prefix" prefix)
     (gascity-records-serialize-maybe-string "provider" provider)
     (gascity-records-serialize-maybe-string "session_template" session-template)
     (gascity-records-serialize-string-or-gexp "start_command" start-command)
     (gascity-records-serialize-tri-state "suspended" suspended)
     (gascity-records-serialize-tri-state "suspended_on_start" suspended-on-start)
     (gascity-records-serialize-maybe-string "timezone" timezone)
     '())))
