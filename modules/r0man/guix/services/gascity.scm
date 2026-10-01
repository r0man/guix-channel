;;; gascity.scm --- Gas City records and TOML serialization
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
;;; The record model and TOML serialization layer for the Gas City service.
;;; This is the Phase 2 "identity/structure" surface: the service/identity
;;; records (`<gascity-supervisor-configuration>',
;;; `<gascity-city-configuration>', `<gascity-pack-configuration>') and the
;;; structural records the service and the worked examples need.  The full
;;; `city.toml'/`pack.toml' field surface is generated in Phase 4; fields with
;;; no record yet are reachable through the `extra-config' escape hatch.
;;;
;;; Records are plain `define-record-type*' records (like
;;; `(r0man guix services github-actions)'), not `define-configuration', and
;;; each has an explicit `->toml' procedure that returns an ordered list of
;;; TOML IR nodes for its body.  Declaration order is emission order.
;;;
;;; Conventions:
;;;
;;;   - a Scheme field name is the TOML key with `_' replaced by `-'; where
;;;     Gas City spells an array table in the singular (`[[agent]]',
;;;     `[[named_session]]') the record field is plural (`agents',
;;;     `named-sessions'), the TOML key is not;
;;;   - every field is optional; the absent marker is the symbol `unset';
;;;   - strings that may embed a store path are `maybe-string-or-gexp', i.e. a
;;;     string, a gexp, or `unset' (the IR treats a gexp as a first-class TOML
;;;     value kind and splices it between the surrounding quotes);
;;;   - tri-state booleans use `unset'/`#t'/`#f'; the serializer emits nothing,
;;;     `true' or `false';
;;;   - string maps are alists `(("K" . "v") ...)' serialized as TOML inline
;;;     tables; ordered lists of strings serialize to TOML arrays.
;;;
;;; The escape hatches are:
;;;
;;;   - `extra-config', a list of TOML IR nodes layered over the generated
;;;     document by `gascity-merge-extra-config' before emission: a field at
;;;     the same full TOML key replaces the generated field, a table merges
;;;     recursively, and a key declared twice in `extra-config' is an error.
;;;     The emitter itself always rejects duplicate keys and knows nothing
;;;     about escape hatches;
;;;   - `extra-toml', a raw string or a file-like appended verbatim after the
;;;     generated document.  A verbatim append can only add whole new tables;
;;;     it cannot add a root scalar after a table header or extend a table the
;;;     generator already emitted (that would be a duplicate-table error);
;;;   - `files' and `rig-files', `(destination . file-like)' pairs that the
;;;     Phase 3 activation copies into the city and rig roots.  They are data
;;;     only here; they are never emitted to TOML.
;;;
;;; No literal secret may appear in a record, the generated TOML, `files', or
;;; the shepherd `environment-variables': the document builders call
;;; `gascity-sanitize-and-validate', which rejects obviously literal secrets
;;; (`sk-ant-...', `ghp_...', long high-entropy tokens, ...) and accepts
;;; `$VAR' references and ordinary paths.  The actual values reach the
;;; supervisor process from the `secrets-file' at runtime.
;;;
;;; Code:

(define-module (r0man guix services gascity)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages version-control)
  #:use-module ((gnu services) #:hide (delete))
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (r0man guix packages task-management)
  #:use-module (r0man guix toml)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:use-module (srfi srfi-34)
  #:export (gascity-tri-state?
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
            gascity-supervisor-configuration-dolt-user-name
            gascity-supervisor-configuration-dolt-user-email
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
            gascity-supervisor-dolt-user-name
            gascity-supervisor-dolt-user-email
            gascity-supervisor-dolt-identity-explicit?
            gascity-supervisor-dolt-config-file
            gascity-supervisor-dolt-config-global
            gascity-dolt-config-global-json
            gascity-json-string
            gascity-supervisor-log-file
            gascity-supervisor-environment
            gascity-city-directory
            gascity-city-site->toml-string
            gascity-cities-toml-merge
            gascity-supervisor-activation-program
            gascity-supervisor-provision-program
            gascity-supervisor-program))


;;;
;;; Field value conventions.
;;;

(define (gascity-tri-state? value)
  "Return #t if VALUE is a tri-state boolean: `unset', #t or #f."
  (memq value '(unset #t #f)))

(define (gascity-maybe-string? value)
  "Return #t if VALUE is a string or `unset'."
  (or (eq? value 'unset) (string? value)))

(define (gascity-maybe-string-or-gexp? value)
  "Return #t if VALUE is a string, a gexp or `unset'."
  (or (eq? value 'unset) (string? value) (gexp? value)))


;;;
;;; Serializer helpers.
;;;

;; Each helper returns an ordered list of TOML IR nodes for one field, or the
;; empty list when the field is absent.  The document builder appends the
;; lists, so declaration order stays emission order.  The gexp-valued string
;; field is the one place a gexp enters the IR: the emitter splices it between
;; the surrounding quotes (see `(r0man guix toml)').

(define (gascity-serialize-string key value)
  "Return a field node for KEY with the string VALUE."
  (list (toml-field key value)))

(define (gascity-serialize-maybe-string key value)
  "Return a field node for KEY with VALUE, or nothing when VALUE is `unset'."
  (if (eq? value 'unset)
      '()
      (list (toml-field key value))))

(define (gascity-serialize-string-or-gexp key value)
  "Return a field node for KEY with VALUE, a string or a gexp, or nothing when
VALUE is `unset'."
  (cond
   ((eq? value 'unset) '())
   ((or (string? value) (gexp? value)) (list (toml-field key value)))
   (else
    (raise (formatted-message
            (G_ "gascity: field '~a' must be a string, a gexp or 'unset, got ~s")
            key value)))))

(define (gascity-serialize-boolean key value)
  "Return a boolean field node for KEY."
  (list (toml-field key (if value #t #f))))

(define (gascity-serialize-maybe-boolean key value)
  "Return a boolean field node for KEY, or nothing when VALUE is `unset'."
  (if (eq? value 'unset) '() (gascity-serialize-boolean key value)))

(define (gascity-serialize-tri-state key value)
  "Return a boolean field node for KEY, or nothing when VALUE is `unset'."
  (if (eq? value 'unset) '() (gascity-serialize-boolean key value)))

(define (gascity-serialize-integer key value)
  "Return an integer field node for KEY."
  (list (toml-field key value)))

(define (gascity-serialize-maybe-integer key value)
  "Return an integer field node for KEY, or nothing when VALUE is `unset'."
  (if (eq? value 'unset) '() (list (toml-field key value))))

(define (gascity-serialize-real key value)
  "Return a real field node for KEY."
  (list (toml-field key value)))

(define (gascity-serialize-maybe-real key value)
  "Return a real field node for KEY, or nothing when VALUE is `unset'."
  (if (eq? value 'unset) '() (list (toml-field key value))))

(define (gascity-serialize-list-of-strings key value)
  "Return an array field node for KEY with the list VALUE, or nothing when
VALUE is `unset'."
  (if (eq? value 'unset)
      '()
      (list (toml-field key (apply toml-array value)))))

(define (gascity-serialize-string-map key value)
  "Return an inline-table field node for KEY with the alist VALUE, or nothing
when VALUE is `unset'."
  (if (eq? value 'unset)
      '()
      (list (toml-field key (apply toml-inline-table value)))))

(define (gascity-serialize-subtable key value proc)
  "Return a `[KEY]' table node whose body is (PROC VALUE), or nothing when
VALUE is `unset' or has an empty body."
  (if (eq? value 'unset)
      '()
      (let ((nodes (proc value)))
        (if (null? nodes) '() (list (apply toml-table key nodes))))))

(define (gascity-serialize-subtables key values proc)
  "Return a `[[KEY]]' array-of-tables node with one element per element of
VALUES, each element the body (PROC VALUE), or nothing when VALUES is `unset'
or empty."
  (if (or (eq? values 'unset) (null? values))
      '()
      (list (apply toml-array-of-tables key (map proc values)))))

(define (gascity-serialize-alist-subtables key value proc)
  "Return a `[KEY]' table node holding one `[KEY.NAME]' child table per
`(NAME . ITEM)' pair of the alist VALUE, each child the body (PROC ITEM), or
nothing when VALUE is `unset' or empty."
  (if (or (eq? value 'unset) (null? value))
      '()
      (list (apply toml-table key
                   (map (lambda (entry)
                          (apply toml-table (car entry) (proc (cdr entry))))
                        value)))))

(define (gascity-empty-serializer . _)
  "Return nothing; a serializer for fields that own no TOML key (a package, a
directory)."
  '())


;;;
;;; The escape-hatch precedence merge.
;;;

;; The merge is an explicit, pure pre-emission pass over the generated node
;; tree.  It produces a clean tree; the emitter never learns about escape
;; hatches and always rejects duplicate keys.

(define (gascity-node-key node)
  "Return the TOML key of NODE, a field, table or array-of-tables node."
  (cond
   ((toml-field? node) (toml-field-key node))
   ((toml-table? node) (toml-table-key node))
   ((toml-array-of-tables? node) (toml-array-of-tables-key node))
   (else
    (raise (formatted-message (G_ "gascity: not a TOML node: ~s") node)))))

(define (gascity-duplicate-keys nodes)
  "Return, once each, the keys of NODES that appear more than once."
  (let loop ((nodes nodes) (seen '()) (duplicates '()))
    (if (null? nodes)
        (reverse duplicates)
        (let ((key (gascity-node-key (car nodes))))
          (loop (cdr nodes)
                (cons key seen)
                (if (and (member key seen) (not (member key duplicates)))
                    (cons key duplicates)
                    duplicates))))))

(define (gascity-merge-level base extra)
  "Return BASE, a list of TOML nodes, with EXTRA layered over it: an EXTRA field
replaces a BASE field with the same key, an EXTRA table merges recursively with
a BASE table of the same key, and any other EXTRA node replaces the BASE node
of the same key.  Raise when EXTRA declares the same key twice at one level."
  (let ((duplicates (gascity-duplicate-keys extra)))
    (unless (null? duplicates)
      (raise (formatted-message
              (G_ "gascity: extra-config declares key '~a' more than once at \
the same level")
              (car duplicates)))))
  (append
   (map (lambda (node)
          (let ((match (find (lambda (candidate)
                               (equal? (gascity-node-key candidate)
                                       (gascity-node-key node)))
                             extra)))
            (cond
             ((not match) node)
             ((and (toml-table? node) (toml-table? match))
              (apply toml-table (toml-table-key node)
                     (gascity-merge-level (toml-table-nodes node)
                                          (toml-table-nodes match))))
             (else match))))
        base)
   (filter (lambda (node)
             (not (any (lambda (candidate)
                         (equal? (gascity-node-key candidate)
                                 (gascity-node-key node)))
                       base)))
           extra)))

(define (gascity-merge-extra-config nodes extra)
  "Return NODES, a generated list of TOML nodes, with EXTRA, the `extra-config'
list of TOML nodes, layered over it (see `gascity-merge-level')."
  (if (null? extra)
      nodes
      (gascity-merge-level nodes extra)))


;;;
;;; The literal-secret guard.
;;;

;; A conservative guard: it rejects the well-known credential prefixes and
;; long opaque tokens, and accepts `$VAR' references, ordinary paths, `sha:'
;; pins and `builtin:' names.  It is deliberately narrow to avoid false
;; positives in the Guix store path and environment-variable surface.

(define %gascity-secret-prefixes
  ;; Credential families whose prefix is unambiguous: Anthropic, GitHub
  ;; personal-access/other tokens, Slack, GitLab, Google API keys and AWS
  ;; access key IDs.
  (list "sk-ant-" "ghp_" "gho_" "ghu_" "ghs_" "ghr_" "github_pat_"
        "xoxb-" "xoxp-" "xoxa-" "xoxr-" "xoxs-" "glpat-" "AIza" "AKIA"))

(define (gascity-secret-character? character)
  "Return #t if CHARACTER may appear in an opaque token."
  (or (char-alphabetic? character)
      (char-numeric? character)
      (memv character '(#\_ #\+ #\- #\=))))

(define (gascity-high-entropy-token? string)
  "Return #t if STRING looks like an opaque credential: long, free of
path/URL/namespace punctuation, and mixing letters and digits."
  (and (> (string-length string) 40)
       (string-every gascity-secret-character? string)
       (string-any char-numeric? string)
       (string-any char-alphabetic? string)))

(define (gascity-secret-value? value)
  "Return #t if VALUE, a string, is an obviously literal secret.  A `NAME=value'
assignment (an environment-variable entry) is judged on its value part; a value
that is a `$VAR' reference is not a secret."
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
                       %gascity-secret-prefixes)
                  (gascity-high-entropy-token? string))))))

(define (gascity-walk-secrets thing)
  "Raise when THING contains an obviously literal secret (see
`gascity-secret-value?').  THING is a TOML node list or any value tree: every
string in it is checked, a gexp is opaque, and lists and alists are walked
recursively without needing to know the TOML IR accessors."
  (cond
   ((string? thing)
    (when (gascity-secret-value? thing)
      (raise (formatted-message
              (G_ "gascity: refusing the literal secret ~s in the \
configuration; write a $VAR reference and let the supervisor resolve it from \
the secrets-file instead")
              thing))))
   ((gexp? thing) #t)
   ((pair? thing)
    (gascity-walk-secrets (car thing))
    (gascity-walk-secrets (cdr thing)))
   (else #t)))

(define (gascity-sanitize-and-validate thing)
  "Raise when THING, a TOML node list or any value tree (for example a list of
`KEY=value' environment-variable strings), contains an obviously literal
secret.  Return #t otherwise.  The document builders call this before
emission."
  (gascity-walk-secrets thing)
  #t)


;;;
;;; Supervisor records.
;;;

(define-record-type* <gascity-supervisor-configuration>
  gascity-supervisor-configuration
  make-gascity-supervisor-configuration
  gascity-supervisor-configuration?
  (id                  gascity-supervisor-configuration-id
                       (default 'unset))            ;string or 'unset
  (package             gascity-supervisor-configuration-package
                       (default gascity-next))       ;package or file-like
  (user                gascity-supervisor-configuration-user
                       (default 'unset))            ;string or 'unset
  (group               gascity-supervisor-configuration-group
                       (default 'unset))            ;string or 'unset
  (gc-home             gascity-supervisor-configuration-gc-home
                       (default 'unset))            ;string or 'unset
  (state-directory     gascity-supervisor-configuration-state-directory
                       (default 'unset))            ;string or 'unset
  (log-directory       gascity-supervisor-configuration-log-directory
                       (default 'unset))            ;string or 'unset
  (bind                gascity-supervisor-configuration-bind
                       (default "127.0.0.1"))        ;string
  (port                gascity-supervisor-configuration-port
                       (default 'unset))            ;integer or 'unset
  (settings            gascity-supervisor-configuration-settings
                       (default 'unset))            ;settings record or 'unset
  (packages            gascity-supervisor-configuration-packages
                       (default '()))               ;list of file-likes
  (environment-variables
   gascity-supervisor-configuration-environment-variables
   (default '()))                                   ;list of "KEY=value" strings
  (secrets-file        gascity-supervisor-configuration-secrets-file
                       (default 'unset))            ;string, gexp or 'unset
  (dolt-user-name      gascity-supervisor-configuration-dolt-user-name
                       (default 'unset))            ;string or 'unset
  (dolt-user-email     gascity-supervisor-configuration-dolt-user-email
                       (default 'unset))            ;string or 'unset
  (cities              gascity-supervisor-configuration-cities
                       (default '())))              ;list of city records

;; The `[supervisor]' fields and the settings record as a whole are service
;; configuration: only the settings record owns a TOML document.

(define-record-type* <gascity-supervisor-settings-configuration>
  gascity-supervisor-settings-configuration
  make-gascity-supervisor-settings-configuration
  gascity-supervisor-settings-configuration?
  (port                       gascity-supervisor-settings-configuration-port
                              (default 'unset))
  (bind                       gascity-supervisor-settings-configuration-bind
                              (default 'unset))
  (patrol-interval
   gascity-supervisor-settings-configuration-patrol-interval
   (default 'unset))
  (allow-mutations?
   gascity-supervisor-settings-configuration-allow-mutations?
   (default 'unset))
  (allowed-origins            gascity-supervisor-settings-configuration-allowed-origins
                              (default 'unset))
  (allowed-hosts              gascity-supervisor-settings-configuration-allowed-hosts
                              (default 'unset))
  (write-auth-verify-key
   gascity-supervisor-settings-configuration-write-auth-verify-key
   (default 'unset))
  (write-auth-required?
   gascity-supervisor-settings-configuration-write-auth-required?
   (default 'unset))
  (write-auth-allow-unverified?
   gascity-supervisor-settings-configuration-write-auth-allow-unverified?
   (default 'unset))
  (read-auth-verify-key
   gascity-supervisor-settings-configuration-read-auth-verify-key
   (default 'unset))
  (read-auth-required?
   gascity-supervisor-settings-configuration-read-auth-required?
   (default 'unset))
  (publication-provider
   gascity-supervisor-settings-configuration-publication-provider
   (default 'unset))
  (publication-tenant-slug
   gascity-supervisor-settings-configuration-publication-tenant-slug
   (default 'unset))
  (publication-public-base-domain
   gascity-supervisor-settings-configuration-publication-public-base-domain
   (default 'unset))
  (publication-tenant-base-domain
   gascity-supervisor-settings-configuration-publication-tenant-base-domain
   (default 'unset))
  (publication-tenant-auth-policy-ref
   gascity-supervisor-settings-configuration-publication-tenant-auth-policy-ref
   (default 'unset))
  (events-export-endpoint
   gascity-supervisor-settings-configuration-events-export-endpoint
   (default 'unset))
  (events-export-cities
   gascity-supervisor-settings-configuration-events-export-cities
   (default 'unset))
  (events-export-token
   gascity-supervisor-settings-configuration-events-export-token
   (default 'unset))
  (events-export-token-file
   gascity-supervisor-settings-configuration-events-export-token-file
   (default 'unset))
  (events-export-actor-salt
   gascity-supervisor-settings-configuration-events-export-actor-salt
   (default 'unset))
  (events-export-batch-max-events
   gascity-supervisor-settings-configuration-events-export-batch-max-events
   (default 'unset))
  (events-export-batch-interval
   gascity-supervisor-settings-configuration-events-export-batch-interval
   (default 'unset))
  (events-export-export-ref
   gascity-supervisor-settings-configuration-events-export-export-ref
   (default 'unset)))

(define (gascity-supervisor-settings-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-supervisor-settings-configuration>: the `[supervisor]' and
`[publication]' tables and the `[events.export]' table."
  (match-record config <gascity-supervisor-settings-configuration>
    (port bind patrol-interval allow-mutations? allowed-origins allowed-hosts
          write-auth-verify-key write-auth-required?
          write-auth-allow-unverified? read-auth-verify-key read-auth-required?
          publication-provider publication-tenant-slug
          publication-public-base-domain publication-tenant-base-domain
          publication-tenant-auth-policy-ref events-export-endpoint
          events-export-cities events-export-token events-export-token-file
          events-export-actor-salt events-export-batch-max-events
          events-export-batch-interval events-export-export-ref)
    (append
     (gascity-serialize-subtable
      "supervisor"
      (append
       (gascity-serialize-maybe-integer "port" port)
       (gascity-serialize-maybe-string "bind" bind)
       (gascity-serialize-maybe-string "patrol_interval" patrol-interval)
       (gascity-serialize-maybe-boolean "allow_mutations" allow-mutations?)
       (gascity-serialize-list-of-strings "allowed_origins" allowed-origins)
       (gascity-serialize-list-of-strings "allowed_hosts" allowed-hosts)
       (gascity-serialize-maybe-string "write_auth_verify_key"
                                       write-auth-verify-key)
       (gascity-serialize-maybe-boolean "write_auth_required"
                                        write-auth-required?)
       (gascity-serialize-maybe-boolean "write_auth_allow_unverified"
                                        write-auth-allow-unverified?)
       (gascity-serialize-maybe-string "read_auth_verify_key"
                                       read-auth-verify-key)
       (gascity-serialize-maybe-boolean "read_auth_required"
                                        read-auth-required?))
      identity)
     (gascity-serialize-subtable
      "publication"
      (append
       (gascity-serialize-maybe-string "provider" publication-provider)
       (gascity-serialize-maybe-string "tenant_slug" publication-tenant-slug)
       (gascity-serialize-maybe-string "public_base_domain"
                                       publication-public-base-domain)
       (gascity-serialize-maybe-string "tenant_base_domain"
                                       publication-tenant-base-domain)
       (gascity-serialize-subtable
        "tenant_auth"
        (gascity-serialize-maybe-string "policy_ref"
                                        publication-tenant-auth-policy-ref)
        identity))
      identity)
     (gascity-serialize-subtable
      "events"
      (gascity-serialize-subtable
       "export"
       (append
        (gascity-serialize-maybe-string "endpoint" events-export-endpoint)
        (gascity-serialize-list-of-strings "cities" events-export-cities)
        (gascity-serialize-maybe-string "token" events-export-token)
        (gascity-serialize-maybe-string "token_file" events-export-token-file)
        (gascity-serialize-maybe-string "actor_salt" events-export-actor-salt)
        (gascity-serialize-maybe-integer "batch_max_events"
                                         events-export-batch-max-events)
        (gascity-serialize-maybe-string "batch_interval"
                                        events-export-batch-interval)
        (gascity-serialize-tri-state "export_ref" events-export-export-ref))
       identity)
      identity))))

;; The supervisor record has no TOML document of its own; its `environment-
;; variables' are checked for literal secrets by the Phase 3 service.  This
;; `->toml' exists so every record has the same shape and returns the settings
;; body when a settings record is set.
(define (gascity-supervisor-configuration->toml config)
  "Return the TOML nodes for the settings of CONFIG, a
<gascity-supervisor-configuration>, or nothing when it has no settings."
  (let ((settings (gascity-supervisor-configuration-settings config)))
    (if (eq? settings 'unset)
        '()
        (gascity-supervisor-settings-configuration->toml settings))))


;;;
;;; City record.
;;;

;; The identity fields (`name', `directory', `package', `user', `enabled?',
;; `genesis?', `install-packs?', `register?', `files', `rig-files') are service
;; configuration and are not emitted to `city.toml'; the rest are the City
;; schema fields, in schema order.

(define-record-type* <gascity-city-configuration>
  gascity-city-configuration
  make-gascity-city-configuration
  gascity-city-configuration?
  (name              gascity-city-configuration-name)     ;string, required
  (directory         gascity-city-configuration-directory
                     (default 'unset))
  (package           gascity-city-configuration-package
                     (default 'unset))
  (user              gascity-city-configuration-user
                     (default 'unset))
  (enabled?          gascity-city-configuration-enabled?
                     (default #t))
  (genesis?          gascity-city-configuration-genesis?
                     (default #t))
  (install-packs?    gascity-city-configuration-install-packs?
                     (default #f))
  (register?         gascity-city-configuration-register?
                     (default #t))
  (pack              gascity-city-configuration-pack
                     (default 'unset))
  (include           gascity-city-configuration-include
                     (default 'unset))
  (workspace         gascity-city-configuration-workspace
                     (default 'unset))
  (providers         gascity-city-configuration-providers
                     (default 'unset))
  (upstreams         gascity-city-configuration-upstreams
                     (default 'unset))
  (imports           gascity-city-configuration-imports
                     (default 'unset))
  (agents            gascity-city-configuration-agents
                     (default 'unset))
  (named-sessions    gascity-city-configuration-named-sessions
                     (default 'unset))
  (rigs              gascity-city-configuration-rigs
                     (default 'unset))
  (patches           gascity-city-configuration-patches
                     (default 'unset))
  (daemon            gascity-city-configuration-daemon
                     (default 'unset))
  (dolt              gascity-city-configuration-dolt
                     (default 'unset))
  (storage           gascity-city-configuration-storage
                     (default 'unset))
  (beads             gascity-city-configuration-beads
                     (default 'unset))
  (session           gascity-city-configuration-session
                     (default 'unset))
  (session-sleep     gascity-city-configuration-session-sleep
                     (default 'unset))
  (defaults          gascity-city-configuration-defaults
                     (default 'unset))
  (agent-defaults    gascity-city-configuration-agent-defaults
                     (default 'unset))
  (pricing           gascity-city-configuration-pricing
                     (default 'unset))
  (files             gascity-city-configuration-files
                     (default '()))             ;list of (destination . file-like)
  (rig-files         gascity-city-configuration-rig-files
                     (default '()))             ;list of (destination . file-like)
  (extra-config      gascity-city-configuration-extra-config
                     (default '()))             ;list of TOML nodes
  (extra-toml        gascity-city-configuration-extra-toml
                     (default #f)))             ;string, file-like or #f

(define (gascity-city-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-city-configuration>.  The identity fields are service configuration
and are not emitted."
  (match-record config <gascity-city-configuration>
    (include workspace providers upstreams imports agents named-sessions rigs
             patches daemon dolt storage beads session session-sleep defaults
             agent-defaults pricing)
    (append
     (gascity-serialize-list-of-strings "include" include)
     (gascity-serialize-subtable "workspace" workspace
                                 gascity-workspace-configuration->toml)
     (gascity-serialize-alist-subtables "providers" providers
                                        gascity-provider-configuration->toml)
     (gascity-serialize-alist-subtables "upstreams" upstreams
                                        gascity-upstream-configuration->toml)
     (gascity-serialize-alist-subtables "imports" imports
                                        gascity-import-configuration->toml)
     (gascity-serialize-subtables "agent" agents
                                  gascity-agent-configuration->toml)
     (gascity-serialize-subtables "named_session" named-sessions
                                  gascity-named-session-configuration->toml)
     (gascity-serialize-subtables "rigs" rigs
                                  gascity-rig-configuration->toml)
     (gascity-serialize-subtable "patches" patches
                                 gascity-patches-configuration->toml)
     (gascity-serialize-subtable "daemon" daemon
                                 gascity-daemon-configuration->toml)
     (gascity-serialize-subtable "dolt" dolt
                                 gascity-dolt-configuration->toml)
     (gascity-serialize-subtable "storage" storage
                                 gascity-storage-configuration->toml)
     (gascity-serialize-subtable "beads" beads
                                 gascity-beads-configuration->toml)
     (gascity-serialize-subtable "session" session
                                 gascity-session-configuration->toml)
     (gascity-serialize-subtable "session_sleep" session-sleep
                                 gascity-session-sleep-configuration->toml)
     (gascity-serialize-subtable "defaults" defaults
                                 gascity-pack-defaults-configuration->toml)
     (gascity-serialize-subtable "agent_defaults" agent-defaults
                                 gascity-agent-defaults-configuration->toml)
     (gascity-serialize-subtables "pricing" pricing
                                  gascity-model-pricing-configuration->toml))))


;;;
;;; Pack record.
;;;

;; The Gas City root `pack.toml'.  The PackMeta fields (`name', `schema',
;; `version', `requires_gc', `description', `includes', `requires') form the
;; `[pack]' table; the rest are root-level tables and arrays, in schema order.

(define-record-type* <gascity-pack-configuration>
  gascity-pack-configuration
  make-gascity-pack-configuration
  gascity-pack-configuration?
  (name            gascity-pack-configuration-name
                   (default 'unset))
  (schema          gascity-pack-configuration-schema
                   (default 'unset))
  (version         gascity-pack-configuration-version
                   (default 'unset))
  (requires-gc     gascity-pack-configuration-requires-gc
                   (default 'unset))
  (description     gascity-pack-configuration-description
                   (default 'unset))
  (includes        gascity-pack-configuration-includes
                   (default 'unset))
  (requires        gascity-pack-configuration-requires
                   (default 'unset))
  (imports         gascity-pack-configuration-imports
                   (default 'unset))
  (agent-defaults  gascity-pack-configuration-agent-defaults
                   (default 'unset))
  (agents          gascity-pack-configuration-agents
                   (default 'unset))
  (named-sessions  gascity-pack-configuration-named-sessions
                   (default 'unset))
  (providers       gascity-pack-configuration-providers
                   (default 'unset))
  (upstreams       gascity-pack-configuration-upstreams
                   (default 'unset))
  (runtimes        gascity-pack-configuration-runtimes
                   (default 'unset))
  (patches         gascity-pack-configuration-patches
                   (default 'unset))
  (doctor          gascity-pack-configuration-doctor
                   (default 'unset))
  (commands        gascity-pack-configuration-commands
                   (default 'unset))
  (global          gascity-pack-configuration-global
                   (default 'unset))
  (pricing         gascity-pack-configuration-pricing
                   (default 'unset)))

(define (gascity-pack-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-pack-configuration>."
  (match-record config <gascity-pack-configuration>
    (name schema version requires-gc description includes requires imports
          agent-defaults agents named-sessions providers upstreams runtimes
          patches doctor commands global pricing)
    (append
     (gascity-serialize-subtable
      "pack"
      (append
       (gascity-serialize-maybe-string "name" name)
       (gascity-serialize-maybe-integer "schema" schema)
       (gascity-serialize-maybe-string "version" version)
       (gascity-serialize-maybe-string "requires_gc" requires-gc)
       (gascity-serialize-maybe-string "description" description)
       (gascity-serialize-list-of-strings "includes" includes)
       (gascity-serialize-subtables "requires" requires
                                    gascity-pack-requirement-configuration->toml))
      identity)
     (gascity-serialize-alist-subtables "imports" imports
                                        gascity-import-configuration->toml)
     (gascity-serialize-subtable "agent_defaults" agent-defaults
                                 gascity-agent-defaults-configuration->toml)
     (gascity-serialize-subtables "agent" agents
                                  gascity-agent-configuration->toml)
     (gascity-serialize-subtables "named_session" named-sessions
                                  gascity-named-session-configuration->toml)
     (gascity-serialize-alist-subtables "providers" providers
                                        gascity-provider-configuration->toml)
     (gascity-serialize-alist-subtables "upstreams" upstreams
                                        gascity-upstream-configuration->toml)
     (gascity-serialize-alist-subtables "runtimes" runtimes
                                        gascity-pack-runtime-entry-configuration->toml)
     (gascity-serialize-subtable "patches" patches
                                 gascity-pack-patches-configuration->toml)
     (gascity-serialize-subtables "doctor" doctor
                                  gascity-pack-doctor-entry-configuration->toml)
     (gascity-serialize-subtables "commands" commands
                                  gascity-pack-command-entry-configuration->toml)
     (gascity-serialize-subtable "global" global
                                 gascity-pack-global-configuration->toml)
     (gascity-serialize-subtables "pricing" pricing
                                  gascity-model-pricing-configuration->toml))))


;;;
;;; Workspace and provider records.
;;;

(define-record-type* <gascity-workspace-configuration>
  gascity-workspace-configuration
  make-gascity-workspace-configuration
  gascity-workspace-configuration?
  (name                gascity-workspace-configuration-name
                       (default 'unset))
  (prefix              gascity-workspace-configuration-prefix
                       (default 'unset))
  (provider            gascity-workspace-configuration-provider
                       (default 'unset))
  (timezone            gascity-workspace-configuration-timezone
                       (default 'unset))
  (start-command       gascity-workspace-configuration-start-command
                       (default 'unset))
  (suspended?          gascity-workspace-configuration-suspended?
                       (default 'unset))
  (suspended-on-start? gascity-workspace-configuration-suspended-on-start?
                       (default 'unset))
  (max-active-sessions gascity-workspace-configuration-max-active-sessions
                       (default 'unset))
  (session-template    gascity-workspace-configuration-session-template
                       (default 'unset))
  (install-agent-hooks gascity-workspace-configuration-install-agent-hooks
                       (default 'unset))
  (global-fragments    gascity-workspace-configuration-global-fragments
                       (default 'unset))
  (includes            gascity-workspace-configuration-includes
                       (default 'unset))
  (default-rig-includes gascity-workspace-configuration-default-rig-includes
                        (default 'unset))
  (env                 gascity-workspace-configuration-env
                       (default 'unset)))

(define (gascity-workspace-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-workspace-configuration>."
  (match-record config <gascity-workspace-configuration>
    (name prefix provider timezone start-command suspended? suspended-on-start?
          max-active-sessions session-template install-agent-hooks
          global-fragments includes default-rig-includes env)
    (append
     (gascity-serialize-maybe-string "name" name)
     (gascity-serialize-maybe-string "prefix" prefix)
     (gascity-serialize-maybe-string "provider" provider)
     (gascity-serialize-maybe-string "timezone" timezone)
     (gascity-serialize-string-or-gexp "start_command" start-command)
     (gascity-serialize-maybe-boolean "suspended" suspended?)
     (gascity-serialize-maybe-boolean "suspended_on_start" suspended-on-start?)
     (gascity-serialize-maybe-integer "max_active_sessions" max-active-sessions)
     (gascity-serialize-maybe-string "session_template" session-template)
     (gascity-serialize-list-of-strings "install_agent_hooks"
                                        install-agent-hooks)
     (gascity-serialize-list-of-strings "global_fragments" global-fragments)
     (gascity-serialize-list-of-strings "includes" includes)
     (gascity-serialize-list-of-strings "default_rig_includes"
                                        default-rig-includes)
     (gascity-serialize-string-map "env" env))))

(define-record-type* <gascity-provider-configuration>
  gascity-provider-configuration
  make-gascity-provider-configuration
  gascity-provider-configuration?
  (base                    gascity-provider-configuration-base
                           (default 'unset))
  (display-name            gascity-provider-configuration-display-name
                           (default 'unset))
  (command                 gascity-provider-configuration-command
                           (default 'unset))
  (args                    gascity-provider-configuration-args
                           (default 'unset))
  (args-append             gascity-provider-configuration-args-append
                           (default 'unset))
  (options-schema-merge    gascity-provider-configuration-options-schema-merge
                           (default 'unset))
  (prompt-mode             gascity-provider-configuration-prompt-mode
                           (default 'unset))
  (prompt-flag             gascity-provider-configuration-prompt-flag
                           (default 'unset))
  (ready-delay-ms          gascity-provider-configuration-ready-delay-ms
                           (default 'unset))
  (ready-prompt-prefix     gascity-provider-configuration-ready-prompt-prefix
                           (default 'unset))
  (process-names           gascity-provider-configuration-process-names
                           (default 'unset))
  (emits-permission-warning gascity-provider-configuration-emits-permission-warning
                            (default 'unset))
  (accept-startup-dialogs? gascity-provider-configuration-accept-startup-dialogs?
                           (default 'unset))
  (env                     gascity-provider-configuration-env
                           (default 'unset))
  (path-check              gascity-provider-configuration-path-check
                           (default 'unset))
  (supports-acp?           gascity-provider-configuration-supports-acp?
                           (default 'unset))
  (supports-hooks?         gascity-provider-configuration-supports-hooks?
                           (default 'unset))
  (instructions-file       gascity-provider-configuration-instructions-file
                           (default 'unset))
  (resume-flag             gascity-provider-configuration-resume-flag
                           (default 'unset))
  (resume-style            gascity-provider-configuration-resume-style
                           (default 'unset))
  (resume-command          gascity-provider-configuration-resume-command
                           (default 'unset))
  (session-id-flag         gascity-provider-configuration-session-id-flag
                           (default 'unset))
  (fork-flag               gascity-provider-configuration-fork-flag
                           (default 'unset))
  (option-defaults         gascity-provider-configuration-option-defaults
                           (default 'unset))
  (print-args              gascity-provider-configuration-print-args
                           (default 'unset))
  (title-model             gascity-provider-configuration-title-model
                           (default 'unset))
  (acp-command             gascity-provider-configuration-acp-command
                           (default 'unset))
  (acp-args                gascity-provider-configuration-acp-args
                           (default 'unset)))

(define (gascity-provider-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-provider-configuration>."
  (match-record config <gascity-provider-configuration>
    (base display-name command args args-append options-schema-merge prompt-mode
          prompt-flag ready-delay-ms ready-prompt-prefix process-names
          emits-permission-warning accept-startup-dialogs? env path-check
          supports-acp? supports-hooks? instructions-file resume-flag
          resume-style resume-command session-id-flag fork-flag option-defaults
          print-args title-model acp-command acp-args)
    (append
     (gascity-serialize-maybe-string "base" base)
     (gascity-serialize-maybe-string "display_name" display-name)
     (gascity-serialize-string-or-gexp "command" command)
     (gascity-serialize-list-of-strings "args" args)
     (gascity-serialize-list-of-strings "args_append" args-append)
     (gascity-serialize-maybe-string "options_schema_merge"
                                     options-schema-merge)
     (gascity-serialize-maybe-string "prompt_mode" prompt-mode)
     (gascity-serialize-maybe-string "prompt_flag" prompt-flag)
     (gascity-serialize-maybe-integer "ready_delay_ms" ready-delay-ms)
     (gascity-serialize-maybe-string "ready_prompt_prefix" ready-prompt-prefix)
     (gascity-serialize-list-of-strings "process_names" process-names)
     (gascity-serialize-tri-state "emits_permission_warning"
                                  emits-permission-warning)
     (gascity-serialize-maybe-boolean "accept_startup_dialogs"
                                      accept-startup-dialogs?)
     (gascity-serialize-string-map "env" env)
     (gascity-serialize-maybe-string "path_check" path-check)
     (gascity-serialize-maybe-boolean "supports_acp" supports-acp?)
     (gascity-serialize-maybe-boolean "supports_hooks" supports-hooks?)
     (gascity-serialize-maybe-string "instructions_file" instructions-file)
     (gascity-serialize-maybe-string "resume_flag" resume-flag)
     (gascity-serialize-maybe-string "resume_style" resume-style)
     (gascity-serialize-maybe-string "resume_command" resume-command)
     (gascity-serialize-maybe-string "session_id_flag" session-id-flag)
     (gascity-serialize-maybe-string "fork_flag" fork-flag)
     (gascity-serialize-string-map "option_defaults" option-defaults)
     (gascity-serialize-list-of-strings "print_args" print-args)
     (gascity-serialize-maybe-string "title_model" title-model)
     (gascity-serialize-string-or-gexp "acp_command" acp-command)
     (gascity-serialize-list-of-strings "acp_args" acp-args))))

(define-record-type* <gascity-provider-patch-configuration>
  gascity-provider-patch-configuration
  make-gascity-provider-patch-configuration
  gascity-provider-patch-configuration?
  (name                    gascity-provider-patch-configuration-name) ;string
  (base                    gascity-provider-patch-configuration-base
                           (default 'unset))
  (command                 gascity-provider-patch-configuration-command
                           (default 'unset))
  (acp-command             gascity-provider-patch-configuration-acp-command
                           (default 'unset))
  (args                    gascity-provider-patch-configuration-args
                           (default 'unset))
  (acp-args                gascity-provider-patch-configuration-acp-args
                           (default 'unset))
  (args-append             gascity-provider-patch-configuration-args-append
                           (default 'unset))
  (options-schema-merge
   gascity-provider-patch-configuration-options-schema-merge
   (default 'unset))
  (prompt-mode             gascity-provider-patch-configuration-prompt-mode
                           (default 'unset))
  (prompt-flag             gascity-provider-patch-configuration-prompt-flag
                           (default 'unset))
  (ready-delay-ms          gascity-provider-patch-configuration-ready-delay-ms
                           (default 'unset))
  (accept-startup-dialogs?
   gascity-provider-patch-configuration-accept-startup-dialogs?
   (default 'unset))
  (env                     gascity-provider-patch-configuration-env
                           (default 'unset))
  (env-remove              gascity-provider-patch-configuration-env-remove
                           (default 'unset))
  (replace?                gascity-provider-patch-configuration-replace?
                           (default 'unset)))

(define (gascity-provider-patch-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-provider-patch-configuration>."
  (match-record config <gascity-provider-patch-configuration>
    (name base command acp-command args acp-args args-append options-schema-merge
          prompt-mode prompt-flag ready-delay-ms accept-startup-dialogs? env
          env-remove replace?)
    (append
     (gascity-serialize-string "name" name)
     (gascity-serialize-maybe-string "base" base)
     (gascity-serialize-string-or-gexp "command" command)
     (gascity-serialize-string-or-gexp "acp_command" acp-command)
     (gascity-serialize-list-of-strings "args" args)
     (gascity-serialize-list-of-strings "acp_args" acp-args)
     (gascity-serialize-list-of-strings "args_append" args-append)
     (gascity-serialize-maybe-string "options_schema_merge"
                                     options-schema-merge)
     (gascity-serialize-maybe-string "prompt_mode" prompt-mode)
     (gascity-serialize-maybe-string "prompt_flag" prompt-flag)
     (gascity-serialize-maybe-integer "ready_delay_ms" ready-delay-ms)
     (gascity-serialize-maybe-boolean "accept_startup_dialogs"
                                      accept-startup-dialogs?)
     (gascity-serialize-string-map "env" env)
     (gascity-serialize-list-of-strings "env_remove" env-remove)
     (gascity-serialize-maybe-boolean "_replace" replace?))))


;;;
;;; Import record.
;;;

(define-record-type* <gascity-import-configuration>
  gascity-import-configuration
  make-gascity-import-configuration
  gascity-import-configuration?
  (source  gascity-import-configuration-source)          ;string
  (version gascity-import-configuration-version
           (default 'unset)))

(define (gascity-import-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-import-configuration>."
  (match-record config <gascity-import-configuration> (source version)
    (append
     (gascity-serialize-string "source" source)
     (gascity-serialize-maybe-string "version" version))))


;;;
;;; Agent records.
;;;

(define-record-type* <gascity-agent-configuration>
  gascity-agent-configuration
  make-gascity-agent-configuration
  gascity-agent-configuration?
  (name                  gascity-agent-configuration-name)  ;string
  (description           gascity-agent-configuration-description
                         (default 'unset))
  (dir                   gascity-agent-configuration-dir
                         (default 'unset))
  (work-dir              gascity-agent-configuration-work-dir
                         (default 'unset))
  (tmux-alias            gascity-agent-configuration-tmux-alias
                         (default 'unset))
  (scope                 gascity-agent-configuration-scope
                         (default 'unset))
  (suspended?            gascity-agent-configuration-suspended?
                         (default 'unset))
  (pre-start             gascity-agent-configuration-pre-start
                         (default 'unset))
  (prompt-template       gascity-agent-configuration-prompt-template
                         (default 'unset))
  (nudge                 gascity-agent-configuration-nudge
                         (default 'unset))
  (session               gascity-agent-configuration-session
                         (default 'unset))
  (provider              gascity-agent-configuration-provider
                         (default 'unset))
  (upstream              gascity-agent-configuration-upstream
                         (default 'unset))
  (start-command         gascity-agent-configuration-start-command
                         (default 'unset))
  (lifecycle             gascity-agent-configuration-lifecycle
                         (default 'unset))
  (args                  gascity-agent-configuration-args
                         (default 'unset))
  (prompt-mode           gascity-agent-configuration-prompt-mode
                         (default 'unset))
  (prompt-flag           gascity-agent-configuration-prompt-flag
                         (default 'unset))
  (ready-delay-ms        gascity-agent-configuration-ready-delay-ms
                         (default 'unset))
  (ready-prompt-prefix   gascity-agent-configuration-ready-prompt-prefix
                         (default 'unset))
  (process-names         gascity-agent-configuration-process-names
                         (default 'unset))
  (emits-permission-warning gascity-agent-configuration-emits-permission-warning
                            (default 'unset))
  (env                   gascity-agent-configuration-env
                         (default 'unset))
  (option-defaults       gascity-agent-configuration-option-defaults
                         (default 'unset))
  (max-active-sessions   gascity-agent-configuration-max-active-sessions
                         (default 'unset))
  (min-active-sessions   gascity-agent-configuration-min-active-sessions
                         (default 'unset))
  (scale-check           gascity-agent-configuration-scale-check
                         (default 'unset))
  (drain-timeout         gascity-agent-configuration-drain-timeout
                         (default 'unset))
  (on-boot               gascity-agent-configuration-on-boot
                         (default 'unset))
  (on-death              gascity-agent-configuration-on-death
                         (default 'unset))
  (namepool              gascity-agent-configuration-namepool
                         (default 'unset))
  (work-query            gascity-agent-configuration-work-query
                         (default 'unset))
  (sling-query           gascity-agent-configuration-sling-query
                         (default 'unset))
  (idle-timeout          gascity-agent-configuration-idle-timeout
                         (default 'unset))
  (max-session-age       gascity-agent-configuration-max-session-age
                         (default 'unset))
  (max-session-age-jitter gascity-agent-configuration-max-session-age-jitter
                          (default 'unset))
  (assigned-work-defer-limit
   gascity-agent-configuration-assigned-work-defer-limit
   (default 'unset))
  (sleep-after-idle      gascity-agent-configuration-sleep-after-idle
                         (default 'unset))
  (auto-reclaim-stale-claims?
   gascity-agent-configuration-auto-reclaim-stale-claims?
   (default 'unset))
  (install-agent-hooks   gascity-agent-configuration-install-agent-hooks
                         (default 'unset))
  (skills                gascity-agent-configuration-skills
                         (default 'unset))
  (mcp                   gascity-agent-configuration-mcp
                         (default 'unset))
  (hooks-installed?      gascity-agent-configuration-hooks-installed?
                         (default 'unset))
  (session-setup         gascity-agent-configuration-session-setup
                         (default 'unset))
  (session-setup-script  gascity-agent-configuration-session-setup-script
                         (default 'unset))
  (session-live          gascity-agent-configuration-session-live
                         (default 'unset))
  (overlay-dir           gascity-agent-configuration-overlay-dir
                         (default 'unset))
  (default-sling-formula gascity-agent-configuration-default-sling-formula
                         (default 'unset))
  (inject-fragments      gascity-agent-configuration-inject-fragments
                         (default 'unset))
  (append-fragments      gascity-agent-configuration-append-fragments
                         (default 'unset))
  (inject-assigned-skills gascity-agent-configuration-inject-assigned-skills
                          (default 'unset))
  (attach?               gascity-agent-configuration-attach?
                         (default 'unset))
  (depends-on            gascity-agent-configuration-depends-on
                         (default 'unset))
  (resume-command        gascity-agent-configuration-resume-command
                         (default 'unset))
  (wake-mode             gascity-agent-configuration-wake-mode
                         (default 'unset))
  (mouse-mode            gascity-agent-configuration-mouse-mode
                         (default 'unset)))

(define (gascity-agent-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-agent-configuration>."
  (match-record config <gascity-agent-configuration>
    (name description dir work-dir tmux-alias scope suspended? pre-start
          prompt-template nudge session provider upstream start-command
          lifecycle args prompt-mode prompt-flag ready-delay-ms
          ready-prompt-prefix process-names emits-permission-warning env
          option-defaults max-active-sessions min-active-sessions scale-check
          drain-timeout on-boot on-death namepool work-query sling-query
          idle-timeout max-session-age max-session-age-jitter
          assigned-work-defer-limit sleep-after-idle auto-reclaim-stale-claims?
          install-agent-hooks skills mcp hooks-installed? session-setup
          session-setup-script session-live overlay-dir default-sling-formula
          inject-fragments append-fragments inject-assigned-skills attach?
          depends-on resume-command wake-mode mouse-mode)
    (append
     (gascity-serialize-string "name" name)
     (gascity-serialize-maybe-string "description" description)
     (gascity-serialize-maybe-string "dir" dir)
     (gascity-serialize-maybe-string "work_dir" work-dir)
     (gascity-serialize-maybe-string "tmux_alias" tmux-alias)
     (gascity-serialize-maybe-string "scope" scope)
     (gascity-serialize-maybe-boolean "suspended" suspended?)
     (gascity-serialize-list-of-strings "pre_start" pre-start)
     (gascity-serialize-maybe-string "prompt_template" prompt-template)
     (gascity-serialize-maybe-string "nudge" nudge)
     (gascity-serialize-maybe-string "session" session)
     (gascity-serialize-maybe-string "provider" provider)
     (gascity-serialize-maybe-string "upstream" upstream)
     (gascity-serialize-string-or-gexp "start_command" start-command)
     (gascity-serialize-maybe-string "lifecycle" lifecycle)
     (gascity-serialize-list-of-strings "args" args)
     (gascity-serialize-maybe-string "prompt_mode" prompt-mode)
     (gascity-serialize-maybe-string "prompt_flag" prompt-flag)
     (gascity-serialize-maybe-integer "ready_delay_ms" ready-delay-ms)
     (gascity-serialize-maybe-string "ready_prompt_prefix" ready-prompt-prefix)
     (gascity-serialize-list-of-strings "process_names" process-names)
     (gascity-serialize-tri-state "emits_permission_warning"
                                  emits-permission-warning)
     (gascity-serialize-string-map "env" env)
     (gascity-serialize-string-map "option_defaults" option-defaults)
     (gascity-serialize-maybe-integer "max_active_sessions" max-active-sessions)
     (gascity-serialize-maybe-integer "min_active_sessions" min-active-sessions)
     (gascity-serialize-maybe-string "scale_check" scale-check)
     (gascity-serialize-maybe-string "drain_timeout" drain-timeout)
     (gascity-serialize-maybe-string "on_boot" on-boot)
     (gascity-serialize-maybe-string "on_death" on-death)
     (gascity-serialize-maybe-string "namepool" namepool)
     (gascity-serialize-maybe-string "work_query" work-query)
     (gascity-serialize-maybe-string "sling_query" sling-query)
     (gascity-serialize-maybe-string "idle_timeout" idle-timeout)
     (gascity-serialize-maybe-string "max_session_age" max-session-age)
     (gascity-serialize-maybe-string "max_session_age_jitter"
                                     max-session-age-jitter)
     (gascity-serialize-maybe-integer "assigned_work_defer_limit"
                                      assigned-work-defer-limit)
     (gascity-serialize-maybe-string "sleep_after_idle" sleep-after-idle)
     (gascity-serialize-maybe-boolean "auto_reclaim_stale_claims"
                                      auto-reclaim-stale-claims?)
     (gascity-serialize-list-of-strings "install_agent_hooks"
                                        install-agent-hooks)
     (gascity-serialize-list-of-strings "skills" skills)
     (gascity-serialize-list-of-strings "mcp" mcp)
     (gascity-serialize-maybe-boolean "hooks_installed" hooks-installed?)
     (gascity-serialize-list-of-strings "session_setup" session-setup)
     (gascity-serialize-string-or-gexp "session_setup_script"
                                       session-setup-script)
     (gascity-serialize-list-of-strings "session_live" session-live)
     (gascity-serialize-maybe-string "overlay_dir" overlay-dir)
     (gascity-serialize-maybe-string "default_sling_formula"
                                     default-sling-formula)
     (gascity-serialize-list-of-strings "inject_fragments" inject-fragments)
     (gascity-serialize-list-of-strings "append_fragments" append-fragments)
     (gascity-serialize-tri-state "inject_assigned_skills"
                                  inject-assigned-skills)
     (gascity-serialize-maybe-boolean "attach" attach?)
     (gascity-serialize-list-of-strings "depends_on" depends-on)
     (gascity-serialize-string-or-gexp "resume_command" resume-command)
     (gascity-serialize-maybe-string "wake_mode" wake-mode)
     (gascity-serialize-maybe-string "mouse_mode" mouse-mode))))

(define-record-type* <gascity-agent-defaults-configuration>
  gascity-agent-defaults-configuration
  make-gascity-agent-defaults-configuration
  gascity-agent-defaults-configuration?
  (provider             gascity-agent-defaults-configuration-provider
                        (default 'unset))
  (model                gascity-agent-defaults-configuration-model
                        (default 'unset))
  (upstream             gascity-agent-defaults-configuration-upstream
                        (default 'unset))
  (wake-mode            gascity-agent-defaults-configuration-wake-mode
                        (default 'unset))
  (default-sling-formula gascity-agent-defaults-configuration-default-sling-formula
                         (default 'unset))
  (allow-overlay        gascity-agent-defaults-configuration-allow-overlay
                        (default 'unset))
  (allow-env-override   gascity-agent-defaults-configuration-allow-env-override
                        (default 'unset))
  (append-fragments     gascity-agent-defaults-configuration-append-fragments
                        (default 'unset))
  (skills               gascity-agent-defaults-configuration-skills
                        (default 'unset))
  (mcp                  gascity-agent-defaults-configuration-mcp
                        (default 'unset)))

(define (gascity-agent-defaults-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-agent-defaults-configuration>."
  (match-record config <gascity-agent-defaults-configuration>
    (provider model upstream wake-mode default-sling-formula allow-overlay
              allow-env-override append-fragments skills mcp)
    (append
     (gascity-serialize-maybe-string "provider" provider)
     (gascity-serialize-maybe-string "model" model)
     (gascity-serialize-maybe-string "upstream" upstream)
     (gascity-serialize-maybe-string "wake_mode" wake-mode)
     (gascity-serialize-maybe-string "default_sling_formula"
                                     default-sling-formula)
     (gascity-serialize-list-of-strings "allow_overlay" allow-overlay)
     (gascity-serialize-list-of-strings "allow_env_override"
                                        allow-env-override)
     (gascity-serialize-list-of-strings "append_fragments" append-fragments)
     (gascity-serialize-list-of-strings "skills" skills)
     (gascity-serialize-list-of-strings "mcp" mcp))))

(define-record-type* <gascity-agent-override-configuration>
  gascity-agent-override-configuration
  make-gascity-agent-override-configuration
  gascity-agent-override-configuration?
  (agent                 gascity-agent-override-configuration-agent) ;string
  (dir                   gascity-agent-override-configuration-dir
                         (default 'unset))
  (work-dir              gascity-agent-override-configuration-work-dir
                         (default 'unset))
  (tmux-alias            gascity-agent-override-configuration-tmux-alias
                         (default 'unset))
  (scope                 gascity-agent-override-configuration-scope
                         (default 'unset))
  (suspended?            gascity-agent-override-configuration-suspended?
                         (default 'unset))
  (env                   gascity-agent-override-configuration-env
                         (default 'unset))
  (env-remove            gascity-agent-override-configuration-env-remove
                         (default 'unset))
  (pre-start             gascity-agent-override-configuration-pre-start
                         (default 'unset))
  (prompt-template       gascity-agent-override-configuration-prompt-template
                         (default 'unset))
  (session               gascity-agent-override-configuration-session
                         (default 'unset))
  (provider              gascity-agent-override-configuration-provider
                         (default 'unset))
  (upstream              gascity-agent-override-configuration-upstream
                         (default 'unset))
  (args                  gascity-agent-override-configuration-args
                         (default 'unset))
  (start-command         gascity-agent-override-configuration-start-command
                         (default 'unset))
  (lifecycle             gascity-agent-override-configuration-lifecycle
                         (default 'unset))
  (nudge                 gascity-agent-override-configuration-nudge
                         (default 'unset))
  (idle-timeout          gascity-agent-override-configuration-idle-timeout
                         (default 'unset))
  (max-session-age       gascity-agent-override-configuration-max-session-age
                         (default 'unset))
  (max-session-age-jitter
   gascity-agent-override-configuration-max-session-age-jitter
   (default 'unset))
  (assigned-work-defer-limit
   gascity-agent-override-configuration-assigned-work-defer-limit
   (default 'unset))
  (sleep-after-idle      gascity-agent-override-configuration-sleep-after-idle
                         (default 'unset))
  (auto-reclaim-stale-claims?
   gascity-agent-override-configuration-auto-reclaim-stale-claims?
   (default 'unset))
  (install-agent-hooks   gascity-agent-override-configuration-install-agent-hooks
                         (default 'unset))
  (skills                gascity-agent-override-configuration-skills
                         (default 'unset))
  (mcp                   gascity-agent-override-configuration-mcp
                         (default 'unset))
  (hooks-installed?      gascity-agent-override-configuration-hooks-installed?
                         (default 'unset))
  (inject-assigned-skills
   gascity-agent-override-configuration-inject-assigned-skills
   (default 'unset))
  (session-setup         gascity-agent-override-configuration-session-setup
                         (default 'unset))
  (session-setup-script  gascity-agent-override-configuration-session-setup-script
                         (default 'unset))
  (session-live          gascity-agent-override-configuration-session-live
                         (default 'unset))
  (overlay-dir           gascity-agent-override-configuration-overlay-dir
                         (default 'unset))
  (default-sling-formula gascity-agent-override-configuration-default-sling-formula
                         (default 'unset))
  (inject-fragments      gascity-agent-override-configuration-inject-fragments
                         (default 'unset))
  (append-fragments      gascity-agent-override-configuration-append-fragments
                         (default 'unset))
  (pre-start-append      gascity-agent-override-configuration-pre-start-append
                         (default 'unset))
  (session-setup-append  gascity-agent-override-configuration-session-setup-append
                         (default 'unset))
  (session-live-append   gascity-agent-override-configuration-session-live-append
                         (default 'unset))
  (install-agent-hooks-append
   gascity-agent-override-configuration-install-agent-hooks-append
   (default 'unset))
  (skills-append         gascity-agent-override-configuration-skills-append
                         (default 'unset))
  (mcp-append            gascity-agent-override-configuration-mcp-append
                         (default 'unset))
  (inject-fragments-append
   gascity-agent-override-configuration-inject-fragments-append
   (default 'unset))
  (attach?               gascity-agent-override-configuration-attach?
                         (default 'unset))
  (depends-on            gascity-agent-override-configuration-depends-on
                         (default 'unset))
  (resume-command        gascity-agent-override-configuration-resume-command
                         (default 'unset))
  (wake-mode             gascity-agent-override-configuration-wake-mode
                         (default 'unset))
  (mouse-mode            gascity-agent-override-configuration-mouse-mode
                         (default 'unset))
  (max-active-sessions   gascity-agent-override-configuration-max-active-sessions
                         (default 'unset))
  (min-active-sessions   gascity-agent-override-configuration-min-active-sessions
                         (default 'unset))
  (scale-check           gascity-agent-override-configuration-scale-check
                         (default 'unset))
  (option-defaults       gascity-agent-override-configuration-option-defaults
                         (default 'unset)))

(define (gascity-agent-override-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-agent-override-configuration>."
  (match-record config <gascity-agent-override-configuration>
    (agent dir work-dir tmux-alias scope suspended? env env-remove pre-start
           prompt-template session provider upstream args start-command
           lifecycle nudge idle-timeout max-session-age max-session-age-jitter
           assigned-work-defer-limit sleep-after-idle
           auto-reclaim-stale-claims? install-agent-hooks skills mcp
           hooks-installed? inject-assigned-skills session-setup
           session-setup-script session-live overlay-dir default-sling-formula
           inject-fragments append-fragments pre-start-append
           session-setup-append session-live-append install-agent-hooks-append
           skills-append mcp-append inject-fragments-append attach? depends-on
           resume-command wake-mode mouse-mode max-active-sessions
           min-active-sessions scale-check option-defaults)
    (append
     (gascity-serialize-string "agent" agent)
     (gascity-serialize-maybe-string "dir" dir)
     (gascity-serialize-maybe-string "work_dir" work-dir)
     (gascity-serialize-maybe-string "tmux_alias" tmux-alias)
     (gascity-serialize-maybe-string "scope" scope)
     (gascity-serialize-maybe-boolean "suspended" suspended?)
     (gascity-serialize-string-map "env" env)
     (gascity-serialize-list-of-strings "env_remove" env-remove)
     (gascity-serialize-list-of-strings "pre_start" pre-start)
     (gascity-serialize-maybe-string "prompt_template" prompt-template)
     (gascity-serialize-maybe-string "session" session)
     (gascity-serialize-maybe-string "provider" provider)
     (gascity-serialize-maybe-string "upstream" upstream)
     (gascity-serialize-list-of-strings "args" args)
     (gascity-serialize-string-or-gexp "start_command" start-command)
     (gascity-serialize-maybe-string "lifecycle" lifecycle)
     (gascity-serialize-maybe-string "nudge" nudge)
     (gascity-serialize-maybe-string "idle_timeout" idle-timeout)
     (gascity-serialize-maybe-string "max_session_age" max-session-age)
     (gascity-serialize-maybe-string "max_session_age_jitter"
                                     max-session-age-jitter)
     (gascity-serialize-maybe-integer "assigned_work_defer_limit"
                                      assigned-work-defer-limit)
     (gascity-serialize-maybe-string "sleep_after_idle" sleep-after-idle)
     (gascity-serialize-maybe-boolean "auto_reclaim_stale_claims"
                                      auto-reclaim-stale-claims?)
     (gascity-serialize-list-of-strings "install_agent_hooks"
                                        install-agent-hooks)
     (gascity-serialize-list-of-strings "skills" skills)
     (gascity-serialize-list-of-strings "mcp" mcp)
     (gascity-serialize-maybe-boolean "hooks_installed" hooks-installed?)
     (gascity-serialize-tri-state "inject_assigned_skills"
                                  inject-assigned-skills)
     (gascity-serialize-list-of-strings "session_setup" session-setup)
     (gascity-serialize-string-or-gexp "session_setup_script"
                                       session-setup-script)
     (gascity-serialize-list-of-strings "session_live" session-live)
     (gascity-serialize-maybe-string "overlay_dir" overlay-dir)
     (gascity-serialize-maybe-string "default_sling_formula"
                                     default-sling-formula)
     (gascity-serialize-list-of-strings "inject_fragments" inject-fragments)
     (gascity-serialize-list-of-strings "append_fragments" append-fragments)
     (gascity-serialize-list-of-strings "pre_start_append" pre-start-append)
     (gascity-serialize-list-of-strings "session_setup_append"
                                        session-setup-append)
     (gascity-serialize-list-of-strings "session_live_append"
                                        session-live-append)
     (gascity-serialize-list-of-strings "install_agent_hooks_append"
                                        install-agent-hooks-append)
     (gascity-serialize-list-of-strings "skills_append" skills-append)
     (gascity-serialize-list-of-strings "mcp_append" mcp-append)
     (gascity-serialize-list-of-strings "inject_fragments_append"
                                        inject-fragments-append)
     (gascity-serialize-maybe-boolean "attach" attach?)
     (gascity-serialize-list-of-strings "depends_on" depends-on)
     (gascity-serialize-string-or-gexp "resume_command" resume-command)
     (gascity-serialize-maybe-string "wake_mode" wake-mode)
     (gascity-serialize-maybe-string "mouse_mode" mouse-mode)
     (gascity-serialize-maybe-integer "max_active_sessions" max-active-sessions)
     (gascity-serialize-maybe-integer "min_active_sessions" min-active-sessions)
     (gascity-serialize-maybe-string "scale_check" scale-check)
     (gascity-serialize-string-map "option_defaults" option-defaults))))

(define-record-type* <gascity-agent-patch-configuration>
  gascity-agent-patch-configuration
  make-gascity-agent-patch-configuration
  gascity-agent-patch-configuration?
  (name                  gascity-agent-patch-configuration-name)  ;string
  (dir                   gascity-agent-patch-configuration-dir
                         (default 'unset))
  (rig                   gascity-agent-patch-configuration-rig
                         (default 'unset))
  (work-dir              gascity-agent-patch-configuration-work-dir
                         (default 'unset))
  (tmux-alias            gascity-agent-patch-configuration-tmux-alias
                         (default 'unset))
  (scope                 gascity-agent-patch-configuration-scope
                         (default 'unset))
  (suspended?            gascity-agent-patch-configuration-suspended?
                         (default 'unset))
  (env                   gascity-agent-patch-configuration-env
                         (default 'unset))
  (env-remove            gascity-agent-patch-configuration-env-remove
                         (default 'unset))
  (pre-start             gascity-agent-patch-configuration-pre-start
                         (default 'unset))
  (prompt-template       gascity-agent-patch-configuration-prompt-template
                         (default 'unset))
  (session               gascity-agent-patch-configuration-session
                         (default 'unset))
  (provider              gascity-agent-patch-configuration-provider
                         (default 'unset))
  (upstream              gascity-agent-patch-configuration-upstream
                         (default 'unset))
  (args                  gascity-agent-patch-configuration-args
                         (default 'unset))
  (start-command         gascity-agent-patch-configuration-start-command
                         (default 'unset))
  (lifecycle             gascity-agent-patch-configuration-lifecycle
                         (default 'unset))
  (nudge                 gascity-agent-patch-configuration-nudge
                         (default 'unset))
  (idle-timeout          gascity-agent-patch-configuration-idle-timeout
                         (default 'unset))
  (max-session-age       gascity-agent-patch-configuration-max-session-age
                         (default 'unset))
  (max-session-age-jitter
   gascity-agent-patch-configuration-max-session-age-jitter
   (default 'unset))
  (assigned-work-defer-limit
   gascity-agent-patch-configuration-assigned-work-defer-limit
   (default 'unset))
  (sleep-after-idle      gascity-agent-patch-configuration-sleep-after-idle
                         (default 'unset))
  (auto-reclaim-stale-claims?
   gascity-agent-patch-configuration-auto-reclaim-stale-claims?
   (default 'unset))
  (install-agent-hooks   gascity-agent-patch-configuration-install-agent-hooks
                         (default 'unset))
  (skills                gascity-agent-patch-configuration-skills
                         (default 'unset))
  (mcp                   gascity-agent-patch-configuration-mcp
                         (default 'unset))
  (skills-append         gascity-agent-patch-configuration-skills-append
                         (default 'unset))
  (mcp-append            gascity-agent-patch-configuration-mcp-append
                         (default 'unset))
  (hooks-installed?      gascity-agent-patch-configuration-hooks-installed?
                         (default 'unset))
  (inject-assigned-skills
   gascity-agent-patch-configuration-inject-assigned-skills
   (default 'unset))
  (session-setup         gascity-agent-patch-configuration-session-setup
                         (default 'unset))
  (session-setup-script  gascity-agent-patch-configuration-session-setup-script
                         (default 'unset))
  (session-live          gascity-agent-patch-configuration-session-live
                         (default 'unset))
  (overlay-dir           gascity-agent-patch-configuration-overlay-dir
                         (default 'unset))
  (default-sling-formula gascity-agent-patch-configuration-default-sling-formula
                         (default 'unset))
  (inject-fragments      gascity-agent-patch-configuration-inject-fragments
                         (default 'unset))
  (append-fragments      gascity-agent-patch-configuration-append-fragments
                         (default 'unset))
  (attach?               gascity-agent-patch-configuration-attach?
                         (default 'unset))
  (depends-on            gascity-agent-patch-configuration-depends-on
                         (default 'unset))
  (resume-command        gascity-agent-patch-configuration-resume-command
                         (default 'unset))
  (wake-mode             gascity-agent-patch-configuration-wake-mode
                         (default 'unset))
  (mouse-mode            gascity-agent-patch-configuration-mouse-mode
                         (default 'unset))
  (pre-start-append      gascity-agent-patch-configuration-pre-start-append
                         (default 'unset))
  (session-setup-append  gascity-agent-patch-configuration-session-setup-append
                         (default 'unset))
  (session-live-append   gascity-agent-patch-configuration-session-live-append
                         (default 'unset))
  (install-agent-hooks-append
   gascity-agent-patch-configuration-install-agent-hooks-append
   (default 'unset))
  (inject-fragments-append
   gascity-agent-patch-configuration-inject-fragments-append
   (default 'unset))
  (max-active-sessions   gascity-agent-patch-configuration-max-active-sessions
                         (default 'unset))
  (min-active-sessions   gascity-agent-patch-configuration-min-active-sessions
                         (default 'unset))
  (scale-check           gascity-agent-patch-configuration-scale-check
                         (default 'unset))
  (option-defaults       gascity-agent-patch-configuration-option-defaults
                         (default 'unset)))

(define (gascity-agent-patch-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-agent-patch-configuration>."
  (match-record config <gascity-agent-patch-configuration>
    (name dir rig work-dir tmux-alias scope suspended? env env-remove pre-start
          prompt-template session provider upstream args start-command lifecycle
          nudge idle-timeout max-session-age max-session-age-jitter
          assigned-work-defer-limit sleep-after-idle auto-reclaim-stale-claims?
          install-agent-hooks skills mcp skills-append mcp-append
          hooks-installed? inject-assigned-skills session-setup
          session-setup-script session-live overlay-dir default-sling-formula
          inject-fragments append-fragments attach? depends-on resume-command
          wake-mode mouse-mode pre-start-append session-setup-append
          session-live-append install-agent-hooks-append
          inject-fragments-append max-active-sessions min-active-sessions
          scale-check option-defaults)
    (append
     (gascity-serialize-string "name" name)
     (gascity-serialize-maybe-string "dir" dir)
     (gascity-serialize-maybe-string "rig" rig)
     (gascity-serialize-maybe-string "work_dir" work-dir)
     (gascity-serialize-maybe-string "tmux_alias" tmux-alias)
     (gascity-serialize-maybe-string "scope" scope)
     (gascity-serialize-maybe-boolean "suspended" suspended?)
     (gascity-serialize-string-map "env" env)
     (gascity-serialize-list-of-strings "env_remove" env-remove)
     (gascity-serialize-list-of-strings "pre_start" pre-start)
     (gascity-serialize-maybe-string "prompt_template" prompt-template)
     (gascity-serialize-maybe-string "session" session)
     (gascity-serialize-maybe-string "provider" provider)
     (gascity-serialize-maybe-string "upstream" upstream)
     (gascity-serialize-list-of-strings "args" args)
     (gascity-serialize-string-or-gexp "start_command" start-command)
     (gascity-serialize-maybe-string "lifecycle" lifecycle)
     (gascity-serialize-maybe-string "nudge" nudge)
     (gascity-serialize-maybe-string "idle_timeout" idle-timeout)
     (gascity-serialize-maybe-string "max_session_age" max-session-age)
     (gascity-serialize-maybe-string "max_session_age_jitter"
                                     max-session-age-jitter)
     (gascity-serialize-maybe-integer "assigned_work_defer_limit"
                                      assigned-work-defer-limit)
     (gascity-serialize-maybe-string "sleep_after_idle" sleep-after-idle)
     (gascity-serialize-maybe-boolean "auto_reclaim_stale_claims"
                                      auto-reclaim-stale-claims?)
     (gascity-serialize-list-of-strings "install_agent_hooks"
                                        install-agent-hooks)
     (gascity-serialize-list-of-strings "skills" skills)
     (gascity-serialize-list-of-strings "mcp" mcp)
     (gascity-serialize-list-of-strings "skills_append" skills-append)
     (gascity-serialize-list-of-strings "mcp_append" mcp-append)
     (gascity-serialize-maybe-boolean "hooks_installed" hooks-installed?)
     (gascity-serialize-tri-state "inject_assigned_skills"
                                  inject-assigned-skills)
     (gascity-serialize-list-of-strings "session_setup" session-setup)
     (gascity-serialize-string-or-gexp "session_setup_script"
                                       session-setup-script)
     (gascity-serialize-list-of-strings "session_live" session-live)
     (gascity-serialize-maybe-string "overlay_dir" overlay-dir)
     (gascity-serialize-maybe-string "default_sling_formula"
                                     default-sling-formula)
     (gascity-serialize-list-of-strings "inject_fragments" inject-fragments)
     (gascity-serialize-list-of-strings "append_fragments" append-fragments)
     (gascity-serialize-maybe-boolean "attach" attach?)
     (gascity-serialize-list-of-strings "depends_on" depends-on)
     (gascity-serialize-string-or-gexp "resume_command" resume-command)
     (gascity-serialize-maybe-string "wake_mode" wake-mode)
     (gascity-serialize-maybe-string "mouse_mode" mouse-mode)
     (gascity-serialize-list-of-strings "pre_start_append" pre-start-append)
     (gascity-serialize-list-of-strings "session_setup_append"
                                        session-setup-append)
     (gascity-serialize-list-of-strings "session_live_append"
                                        session-live-append)
     (gascity-serialize-list-of-strings "install_agent_hooks_append"
                                        install-agent-hooks-append)
     (gascity-serialize-list-of-strings "inject_fragments_append"
                                        inject-fragments-append)
     (gascity-serialize-maybe-integer "max_active_sessions" max-active-sessions)
     (gascity-serialize-maybe-integer "min_active_sessions" min-active-sessions)
     (gascity-serialize-maybe-string "scale_check" scale-check)
     (gascity-serialize-string-map "option_defaults" option-defaults))))


;;;
;;; Named-session, rig, patches and defaults records.
;;;

(define-record-type* <gascity-named-session-configuration>
  gascity-named-session-configuration
  make-gascity-named-session-configuration
  gascity-named-session-configuration?
  (name     gascity-named-session-configuration-name
            (default 'unset))
  (template gascity-named-session-configuration-template)      ;string
  (scope    gascity-named-session-configuration-scope
            (default 'unset))
  (dir      gascity-named-session-configuration-dir
            (default 'unset))
  (mode     gascity-named-session-configuration-mode
            (default 'unset)))

(define (gascity-named-session-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-named-session-configuration>."
  (match-record config <gascity-named-session-configuration>
    (name template scope dir mode)
    (append
     (gascity-serialize-maybe-string "name" name)
     (gascity-serialize-string "template" template)
     (gascity-serialize-maybe-string "scope" scope)
     (gascity-serialize-maybe-string "dir" dir)
     (gascity-serialize-maybe-string "mode" mode))))

(define-record-type* <gascity-rig-configuration>
  gascity-rig-configuration
  make-gascity-rig-configuration
  gascity-rig-configuration?
  (name                  gascity-rig-configuration-name)    ;string
  (path                  gascity-rig-configuration-path
                         (default 'unset))
  (prefix                gascity-rig-configuration-prefix
                         (default 'unset))
  (default-branch        gascity-rig-configuration-default-branch
                         (default 'unset))
  (suspended?            gascity-rig-configuration-suspended?
                         (default 'unset))
  (suspended-on-start?   gascity-rig-configuration-suspended-on-start?
                         (default 'unset))
  (formulas-dir          gascity-rig-configuration-formulas-dir
                         (default 'unset))
  (includes              gascity-rig-configuration-includes
                         (default 'unset))
  (imports               gascity-rig-configuration-imports
                         (default 'unset))
  (max-active-sessions   gascity-rig-configuration-max-active-sessions
                         (default 'unset))
  (overrides             gascity-rig-configuration-overrides
                         (default 'unset))
  (patches               gascity-rig-configuration-patches
                         (default 'unset))
  (default-sling-target  gascity-rig-configuration-default-sling-target
                         (default 'unset))
  (default-sling-targets gascity-rig-configuration-default-sling-targets
                         (default 'unset))
  (session-sleep         gascity-rig-configuration-session-sleep
                         (default 'unset))
  (dolt-host             gascity-rig-configuration-dolt-host
                         (default 'unset))
  (dolt-port             gascity-rig-configuration-dolt-port
                         (default 'unset))
  (formula-vars          gascity-rig-configuration-formula-vars
                         (default 'unset)))

(define (gascity-rig-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-rig-configuration>."
  (match-record config <gascity-rig-configuration>
    (name path prefix default-branch suspended? suspended-on-start? formulas-dir
          includes imports max-active-sessions overrides patches
          default-sling-target default-sling-targets session-sleep dolt-host
          dolt-port formula-vars)
    (append
     (gascity-serialize-string "name" name)
     (gascity-serialize-string-or-gexp "path" path)
     (gascity-serialize-maybe-string "prefix" prefix)
     (gascity-serialize-maybe-string "default_branch" default-branch)
     (gascity-serialize-maybe-boolean "suspended" suspended?)
     (gascity-serialize-maybe-boolean "suspended_on_start" suspended-on-start?)
     (gascity-serialize-maybe-string "formulas_dir" formulas-dir)
     (gascity-serialize-list-of-strings "includes" includes)
     (gascity-serialize-alist-subtables "imports" imports
                                        gascity-import-configuration->toml)
     (gascity-serialize-maybe-integer "max_active_sessions" max-active-sessions)
     (gascity-serialize-subtables "overrides" overrides
                                  gascity-agent-override-configuration->toml)
     (gascity-serialize-subtables "patches" patches
                                  gascity-agent-override-configuration->toml)
     (gascity-serialize-maybe-string "default_sling_target" default-sling-target)
     (gascity-serialize-list-of-strings "default_sling_targets"
                                        default-sling-targets)
     (gascity-serialize-subtable "session_sleep" session-sleep
                                 gascity-session-sleep-configuration->toml)
     (gascity-serialize-maybe-string "dolt_host" dolt-host)
     (gascity-serialize-maybe-string "dolt_port" dolt-port)
     (gascity-serialize-string-map "formula_vars" formula-vars))))

(define-record-type* <gascity-rig-patch-configuration>
  gascity-rig-patch-configuration
  make-gascity-rig-patch-configuration
  gascity-rig-patch-configuration?
  (name                gascity-rig-patch-configuration-name)   ;string
  (path                gascity-rig-patch-configuration-path
                       (default 'unset))
  (prefix              gascity-rig-patch-configuration-prefix
                       (default 'unset))
  (default-branch      gascity-rig-patch-configuration-default-branch
                       (default 'unset))
  (suspended?          gascity-rig-patch-configuration-suspended?
                       (default 'unset))
  (suspended-on-start? gascity-rig-patch-configuration-suspended-on-start?
                       (default 'unset))
  (formula-vars        gascity-rig-patch-configuration-formula-vars
                       (default 'unset)))

(define (gascity-rig-patch-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-rig-patch-configuration>."
  (match-record config <gascity-rig-patch-configuration>
    (name path prefix default-branch suspended? suspended-on-start? formula-vars)
    (append
     (gascity-serialize-string "name" name)
     (gascity-serialize-string-or-gexp "path" path)
     (gascity-serialize-maybe-string "prefix" prefix)
     (gascity-serialize-maybe-string "default_branch" default-branch)
     (gascity-serialize-maybe-boolean "suspended" suspended?)
     (gascity-serialize-maybe-boolean "suspended_on_start" suspended-on-start?)
     (gascity-serialize-string-map "formula_vars" formula-vars))))

;; Only the patch lists whose record is hand-written in this phase are fields;
;; `named_session' (a NamedSessionPatch) and `github_pr_monitor' are reachable
;; through `extra-config' until Phase 4 generates their records.
(define-record-type* <gascity-patches-configuration>
  gascity-patches-configuration
  make-gascity-patches-configuration
  gascity-patches-configuration?
  (agents    gascity-patches-configuration-agents
             (default 'unset))
  (rigs      gascity-patches-configuration-rigs
             (default 'unset))
  (providers gascity-patches-configuration-providers
             (default 'unset)))

(define (gascity-patches-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-patches-configuration>."
  (match-record config <gascity-patches-configuration> (agents rigs providers)
    (append
     (gascity-serialize-subtables "agent" agents
                                  gascity-agent-patch-configuration->toml)
     (gascity-serialize-subtables "rigs" rigs
                                  gascity-rig-patch-configuration->toml)
     (gascity-serialize-subtables "providers" providers
                                  gascity-provider-patch-configuration->toml))))

(define-record-type* <gascity-pack-defaults-configuration>
  gascity-pack-defaults-configuration
  make-gascity-pack-defaults-configuration
  gascity-pack-defaults-configuration?
  (rig gascity-pack-defaults-configuration-rig
       (default 'unset)))

(define (gascity-pack-defaults-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-pack-defaults-configuration>."
  (match-record config <gascity-pack-defaults-configuration> (rig)
    (gascity-serialize-subtable "rig" rig
                                gascity-pack-rig-defaults-configuration->toml)))

(define-record-type* <gascity-pack-rig-defaults-configuration>
  gascity-pack-rig-defaults-configuration
  make-gascity-pack-rig-defaults-configuration
  gascity-pack-rig-defaults-configuration?
  (imports gascity-pack-rig-defaults-configuration-imports
           (default 'unset)))

(define (gascity-pack-rig-defaults-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-pack-rig-defaults-configuration>."
  (match-record config <gascity-pack-rig-defaults-configuration> (imports)
    (gascity-serialize-alist-subtables "imports" imports
                                       gascity-import-configuration->toml)))


;;;
;;; Daemon, dolt, storage, beads and session records.
;;;

(define-record-type* <gascity-daemon-configuration>
  gascity-daemon-configuration
  make-gascity-daemon-configuration
  gascity-daemon-configuration?
  (formula-v2            gascity-daemon-configuration-formula-v2
                         (default 'unset))
  (graph-workflows?      gascity-daemon-configuration-graph-workflows?
                         (default 'unset))
  (patrol-interval       gascity-daemon-configuration-patrol-interval
                         (default 'unset))
  (max-restarts          gascity-daemon-configuration-max-restarts
                         (default 'unset))
  (restart-window        gascity-daemon-configuration-restart-window
                         (default 'unset))
  (session-circuit-breaker?
   gascity-daemon-configuration-session-circuit-breaker?
   (default 'unset))
  (session-circuit-breaker-max-restarts
   gascity-daemon-configuration-session-circuit-breaker-max-restarts
   (default 'unset))
  (session-circuit-breaker-window
   gascity-daemon-configuration-session-circuit-breaker-window
   (default 'unset))
  (session-circuit-breaker-reset-after
   gascity-daemon-configuration-session-circuit-breaker-reset-after
   (default 'unset))
  (shutdown-timeout      gascity-daemon-configuration-shutdown-timeout
                         (default 'unset))
  (dolt-stop-timeout     gascity-daemon-configuration-dolt-stop-timeout
                         (default 'unset))
  (dolt-start-address-in-use-retry-window
   gascity-daemon-configuration-dolt-start-address-in-use-retry-window
   (default 'unset))
  (wisp-gc-interval      gascity-daemon-configuration-wisp-gc-interval
                         (default 'unset))
  (wisp-ttl              gascity-daemon-configuration-wisp-ttl
                         (default 'unset))
  (drift-drain-timeout   gascity-daemon-configuration-drift-drain-timeout
                         (default 'unset))
  (observe-paths         gascity-daemon-configuration-observe-paths
                         (default 'unset))
  (probe-concurrency     gascity-daemon-configuration-probe-concurrency
                         (default 'unset))
  (max-wakes-per-tick    gascity-daemon-configuration-max-wakes-per-tick
                         (default 'unset))
  (nudge-dispatcher      gascity-daemon-configuration-nudge-dispatcher
                         (default 'unset))
  (auto-restart-on-drift? gascity-daemon-configuration-auto-restart-on-drift?
                          (default 'unset))
  (auto-reap-closed-bead-worktrees?
   gascity-daemon-configuration-auto-reap-closed-bead-worktrees?
   (default 'unset))
  (auto-reap-closed-bead-worktrees-dry-run?
   gascity-daemon-configuration-auto-reap-closed-bead-worktrees-dry-run?
   (default 'unset))
  (auto-reap-closed-bead-worktrees-min-age-minutes
   gascity-daemon-configuration-auto-reap-closed-bead-worktrees-min-age-minutes
   (default 'unset))
  (start-ready-timeout   gascity-daemon-configuration-start-ready-timeout
                         (default 'unset))
  (tick-debounce         gascity-daemon-configuration-tick-debounce
                         (default 'unset))
  (auto-prune-worker-dir? gascity-daemon-configuration-auto-prune-worker-dir?
                          (default 'unset)))

(define (gascity-daemon-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-daemon-configuration>.  `formula_v2' is tri-state."
  (match-record config <gascity-daemon-configuration>
    (formula-v2 graph-workflows? patrol-interval max-restarts restart-window
                session-circuit-breaker? session-circuit-breaker-max-restarts
                session-circuit-breaker-window
                session-circuit-breaker-reset-after shutdown-timeout
                dolt-stop-timeout dolt-start-address-in-use-retry-window
                wisp-gc-interval wisp-ttl drift-drain-timeout observe-paths
                probe-concurrency max-wakes-per-tick nudge-dispatcher
                auto-restart-on-drift? auto-reap-closed-bead-worktrees?
                auto-reap-closed-bead-worktrees-dry-run?
                auto-reap-closed-bead-worktrees-min-age-minutes
                start-ready-timeout tick-debounce auto-prune-worker-dir?)
    (append
     (gascity-serialize-tri-state "formula_v2" formula-v2)
     (gascity-serialize-maybe-boolean "graph_workflows" graph-workflows?)
     (gascity-serialize-maybe-string "patrol_interval" patrol-interval)
     (gascity-serialize-maybe-integer "max_restarts" max-restarts)
     (gascity-serialize-maybe-string "restart_window" restart-window)
     (gascity-serialize-maybe-boolean "session_circuit_breaker"
                                      session-circuit-breaker?)
     (gascity-serialize-maybe-integer "session_circuit_breaker_max_restarts"
                                      session-circuit-breaker-max-restarts)
     (gascity-serialize-maybe-string "session_circuit_breaker_window"
                                     session-circuit-breaker-window)
     (gascity-serialize-maybe-string "session_circuit_breaker_reset_after"
                                     session-circuit-breaker-reset-after)
     (gascity-serialize-maybe-string "shutdown_timeout" shutdown-timeout)
     (gascity-serialize-maybe-string "dolt_stop_timeout" dolt-stop-timeout)
     (gascity-serialize-maybe-string "dolt_start_address_in_use_retry_window"
                                     dolt-start-address-in-use-retry-window)
     (gascity-serialize-maybe-string "wisp_gc_interval" wisp-gc-interval)
     (gascity-serialize-maybe-string "wisp_ttl" wisp-ttl)
     (gascity-serialize-maybe-string "drift_drain_timeout" drift-drain-timeout)
     (gascity-serialize-list-of-strings "observe_paths" observe-paths)
     (gascity-serialize-maybe-integer "probe_concurrency" probe-concurrency)
     (gascity-serialize-maybe-integer "max_wakes_per_tick" max-wakes-per-tick)
     (gascity-serialize-maybe-string "nudge_dispatcher" nudge-dispatcher)
     (gascity-serialize-maybe-boolean "auto_restart_on_drift"
                                      auto-restart-on-drift?)
     (gascity-serialize-maybe-boolean "auto_reap_closed_bead_worktrees"
                                      auto-reap-closed-bead-worktrees?)
     (gascity-serialize-maybe-boolean "auto_reap_closed_bead_worktrees_dry_run"
                                      auto-reap-closed-bead-worktrees-dry-run?)
     (gascity-serialize-maybe-integer
      "auto_reap_closed_bead_worktrees_min_age_minutes"
      auto-reap-closed-bead-worktrees-min-age-minutes)
     (gascity-serialize-maybe-string "start_ready_timeout" start-ready-timeout)
     (gascity-serialize-maybe-string "tick_debounce" tick-debounce)
     (gascity-serialize-maybe-boolean "auto_prune_worker_dir"
                                      auto-prune-worker-dir?))))

(define-record-type* <gascity-dolt-configuration>
  gascity-dolt-configuration
  make-gascity-dolt-configuration
  gascity-dolt-configuration?
  (port                  gascity-dolt-configuration-port
                         (default 'unset))
  (host                  gascity-dolt-configuration-host
                         (default 'unset))
  (archive-level         gascity-dolt-configuration-archive-level
                         (default 'unset))
  (auto-gc-enabled?      gascity-dolt-configuration-auto-gc-enabled?
                         (default 'unset))
  (max-connections       gascity-dolt-configuration-max-connections
                         (default 'unset))
  (read-timeout-millis   gascity-dolt-configuration-read-timeout-millis
                         (default 'unset))
  (write-timeout-millis  gascity-dolt-configuration-write-timeout-millis
                         (default 'unset))
  (wait-timeout-seconds  gascity-dolt-configuration-wait-timeout-seconds
                         (default 'unset))
  (dolt-lock-release-timeout
   gascity-dolt-configuration-dolt-lock-release-timeout
   (default 'unset)))

(define (gascity-dolt-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-dolt-configuration>."
  (match-record config <gascity-dolt-configuration>
    (port host archive-level auto-gc-enabled? max-connections
          read-timeout-millis write-timeout-millis wait-timeout-seconds
          dolt-lock-release-timeout)
    (append
     (gascity-serialize-maybe-integer "port" port)
     (gascity-serialize-maybe-string "host" host)
     (gascity-serialize-maybe-integer "archive_level" archive-level)
     (gascity-serialize-maybe-boolean "auto_gc_enabled" auto-gc-enabled?)
     (gascity-serialize-maybe-integer "max_connections" max-connections)
     (gascity-serialize-maybe-integer "read_timeout_millis" read-timeout-millis)
     (gascity-serialize-maybe-integer "write_timeout_millis"
                                      write-timeout-millis)
     (gascity-serialize-maybe-integer "wait_timeout_seconds"
                                      wait-timeout-seconds)
     (gascity-serialize-maybe-string "dolt_lock_release_timeout"
                                     dolt-lock-release-timeout))))

;; `bindings' (a map of name to StorageBindingConfig) is reachable through
;; `extra-config' until Phase 4 generates that record.
(define-record-type* <gascity-storage-configuration>
  gascity-storage-configuration
  make-gascity-storage-configuration
  gascity-storage-configuration?
  (classes gascity-storage-configuration-classes
           (default 'unset)))

(define (gascity-storage-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-storage-configuration>."
  (match-record config <gascity-storage-configuration> (classes)
    (gascity-serialize-subtable "classes" classes
                                gascity-storage-classes-configuration->toml)))

(define-record-type* <gascity-storage-classes-configuration>
  gascity-storage-classes-configuration
  make-gascity-storage-classes-configuration
  gascity-storage-classes-configuration?
  (work       gascity-storage-classes-configuration-work
              (default 'unset))
  (graph      gascity-storage-classes-configuration-graph
              (default 'unset))
  (sessions   gascity-storage-classes-configuration-sessions
              (default 'unset))
  (messaging  gascity-storage-classes-configuration-messaging
              (default 'unset))
  (orders     gascity-storage-classes-configuration-orders
              (default 'unset))
  (nudges     gascity-storage-classes-configuration-nudges
              (default 'unset)))

(define (gascity-storage-classes-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-storage-classes-configuration>."
  (match-record config <gascity-storage-classes-configuration>
    (work graph sessions messaging orders nudges)
    (append
     (gascity-serialize-maybe-string "work" work)
     (gascity-serialize-maybe-string "graph" graph)
     (gascity-serialize-maybe-string "sessions" sessions)
     (gascity-serialize-maybe-string "messaging" messaging)
     (gascity-serialize-maybe-string "orders" orders)
     (gascity-serialize-maybe-string "nudges" nudges))))

(define-record-type* <gascity-beads-configuration>
  gascity-beads-configuration
  make-gascity-beads-configuration
  gascity-beads-configuration?
  (provider           gascity-beads-configuration-provider
                      (default 'unset))
  (backend            gascity-beads-configuration-backend
                      (default 'unset))
  (event-hooks?       gascity-beads-configuration-event-hooks?
                      (default 'unset))
  (bd-compatibility   gascity-beads-configuration-bd-compatibility
                      (default 'unset))
  (conditional-writes gascity-beads-configuration-conditional-writes
                      (default 'unset))
  (guarded-release    gascity-beads-configuration-guarded-release
                      (default 'unset)))

(define (gascity-beads-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-beads-configuration>."
  (match-record config <gascity-beads-configuration>
    (provider backend event-hooks? bd-compatibility conditional-writes
              guarded-release)
    (append
     (gascity-serialize-maybe-string "provider" provider)
     (gascity-serialize-maybe-string "backend" backend)
     (gascity-serialize-maybe-boolean "event_hooks" event-hooks?)
     (gascity-serialize-maybe-string "bd_compatibility" bd-compatibility)
     (gascity-serialize-maybe-string "conditional_writes" conditional-writes)
     (gascity-serialize-maybe-string "guarded_release" guarded-release))))

(define-record-type* <gascity-session-configuration>
  gascity-session-configuration
  make-gascity-session-configuration
  gascity-session-configuration?
  (provider              gascity-session-configuration-provider
                         (default 'unset))
  (setup-timeout         gascity-session-configuration-setup-timeout
                         (default 'unset))
  (setup-max-timeout     gascity-session-configuration-setup-max-timeout
                         (default 'unset))
  (nudge-ready-timeout   gascity-session-configuration-nudge-ready-timeout
                         (default 'unset))
  (nudge-retry-interval  gascity-session-configuration-nudge-retry-interval
                         (default 'unset))
  (nudge-poll-interval   gascity-session-configuration-nudge-poll-interval
                         (default 'unset))
  (nudge-lock-timeout    gascity-session-configuration-nudge-lock-timeout
                         (default 'unset))
  (debounce-ms           gascity-session-configuration-debounce-ms
                         (default 'unset))
  (display-ms            gascity-session-configuration-display-ms
                         (default 'unset))
  (startup-timeout       gascity-session-configuration-startup-timeout
                         (default 'unset))
  (progress-stall-timeout gascity-session-configuration-progress-stall-timeout
                          (default 'unset))
  (claim-holder-stall-timeout
   gascity-session-configuration-claim-holder-stall-timeout
   (default 'unset))
  (socket                gascity-session-configuration-socket
                         (default 'unset))
  (remote-match          gascity-session-configuration-remote-match
                         (default 'unset)))

(define (gascity-session-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-session-configuration>."
  (match-record config <gascity-session-configuration>
    (provider setup-timeout setup-max-timeout nudge-ready-timeout
              nudge-retry-interval nudge-poll-interval nudge-lock-timeout
              debounce-ms display-ms startup-timeout progress-stall-timeout
              claim-holder-stall-timeout socket remote-match)
    (append
     (gascity-serialize-maybe-string "provider" provider)
     (gascity-serialize-maybe-string "setup_timeout" setup-timeout)
     (gascity-serialize-maybe-string "setup_max_timeout" setup-max-timeout)
     (gascity-serialize-maybe-string "nudge_ready_timeout" nudge-ready-timeout)
     (gascity-serialize-maybe-string "nudge_retry_interval"
                                     nudge-retry-interval)
     (gascity-serialize-maybe-string "nudge_poll_interval" nudge-poll-interval)
     (gascity-serialize-maybe-string "nudge_lock_timeout" nudge-lock-timeout)
     (gascity-serialize-maybe-integer "debounce_ms" debounce-ms)
     (gascity-serialize-maybe-integer "display_ms" display-ms)
     (gascity-serialize-maybe-string "startup_timeout" startup-timeout)
     (gascity-serialize-maybe-string "progress_stall_timeout"
                                     progress-stall-timeout)
     (gascity-serialize-maybe-string "claim_holder_stall_timeout"
                                     claim-holder-stall-timeout)
     (gascity-serialize-maybe-string "socket" socket)
     (gascity-serialize-maybe-string "remote_match" remote-match))))

(define-record-type* <gascity-session-sleep-configuration>
  gascity-session-sleep-configuration
  make-gascity-session-sleep-configuration
  gascity-session-sleep-configuration?
  (interactive-resume gascity-session-sleep-configuration-interactive-resume
                      (default 'unset))
  (interactive-fresh  gascity-session-sleep-configuration-interactive-fresh
                      (default 'unset))
  (noninteractive     gascity-session-sleep-configuration-noninteractive
                      (default 'unset)))

(define (gascity-session-sleep-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-session-sleep-configuration>."
  (match-record config <gascity-session-sleep-configuration>
    (interactive-resume interactive-fresh noninteractive)
    (append
     (gascity-serialize-maybe-string "interactive_resume" interactive-resume)
     (gascity-serialize-maybe-string "interactive_fresh" interactive-fresh)
     (gascity-serialize-maybe-string "noninteractive" noninteractive))))

(define-record-type* <gascity-upstream-configuration>
  gascity-upstream-configuration
  make-gascity-upstream-configuration
  gascity-upstream-configuration?
  (description     gascity-upstream-configuration-description
                   (default 'unset))
  (base-url        gascity-upstream-configuration-base-url
                   (default 'unset))
  (api-key         gascity-upstream-configuration-api-key
                   (default 'unset))
  (auth-token      gascity-upstream-configuration-auth-token
                   (default 'unset))
  (base-url-env    gascity-upstream-configuration-base-url-env
                   (default 'unset))
  (api-key-env     gascity-upstream-configuration-api-key-env
                   (default 'unset))
  (auth-token-env  gascity-upstream-configuration-auth-token-env
                   (default 'unset))
  (env             gascity-upstream-configuration-env
                   (default 'unset)))

(define (gascity-upstream-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-upstream-configuration>."
  (match-record config <gascity-upstream-configuration>
    (description base-url api-key auth-token base-url-env api-key-env
                 auth-token-env env)
    (append
     (gascity-serialize-maybe-string "description" description)
     (gascity-serialize-maybe-string "base_url" base-url)
     (gascity-serialize-maybe-string "api_key" api-key)
     (gascity-serialize-maybe-string "auth_token" auth-token)
     (gascity-serialize-maybe-string "base_url_env" base-url-env)
     (gascity-serialize-maybe-string "api_key_env" api-key-env)
     (gascity-serialize-maybe-string "auth_token_env" auth-token-env)
     (gascity-serialize-string-map "env" env))))

(define-record-type* <gascity-model-pricing-configuration>
  gascity-model-pricing-configuration
  make-gascity-model-pricing-configuration
  gascity-model-pricing-configuration?
  (provider      gascity-model-pricing-configuration-provider)   ;string
  (model         gascity-model-pricing-configuration-model)      ;string
  (tier          gascity-model-pricing-configuration-tier
                 (default 'unset))
  (last-verified gascity-model-pricing-configuration-last-verified))

(define (gascity-model-pricing-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-model-pricing-configuration>."
  (match-record config <gascity-model-pricing-configuration>
    (provider model tier last-verified)
    (append
     (gascity-serialize-string "provider" provider)
     (gascity-serialize-string "model" model)
     (gascity-serialize-subtable "tier" tier
                                 gascity-pricing-tier-configuration->toml)
     (gascity-serialize-string "last_verified" last-verified))))

(define-record-type* <gascity-pricing-tier-configuration>
  gascity-pricing-tier-configuration
  make-gascity-pricing-tier-configuration
  gascity-pricing-tier-configuration?
  (prompt-usd-per-1m         gascity-pricing-tier-configuration-prompt-usd-per-1m)
  (completion-usd-per-1m     gascity-pricing-tier-configuration-completion-usd-per-1m)
  (cache-read-usd-per-1m     gascity-pricing-tier-configuration-cache-read-usd-per-1m)
  (cache-creation-usd-per-1m gascity-pricing-tier-configuration-cache-creation-usd-per-1m))

(define (gascity-pricing-tier-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-pricing-tier-configuration>."
  (match-record config <gascity-pricing-tier-configuration>
    (prompt-usd-per-1m completion-usd-per-1m cache-read-usd-per-1m
                       cache-creation-usd-per-1m)
    (append
     (gascity-serialize-real "prompt_usd_per_1m" prompt-usd-per-1m)
     (gascity-serialize-real "completion_usd_per_1m" completion-usd-per-1m)
     (gascity-serialize-real "cache_read_usd_per_1m" cache-read-usd-per-1m)
     (gascity-serialize-real "cache_creation_usd_per_1m"
                             cache-creation-usd-per-1m))))


;;;
;;; Pack leaf records.
;;;

(define-record-type* <gascity-pack-requirement-configuration>
  gascity-pack-requirement-configuration
  make-gascity-pack-requirement-configuration
  gascity-pack-requirement-configuration?
  (scope gascity-pack-requirement-configuration-scope)   ;string
  (agent gascity-pack-requirement-configuration-agent))  ;string

(define (gascity-pack-requirement-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-pack-requirement-configuration>."
  (match-record config <gascity-pack-requirement-configuration> (scope agent)
    (append
     (gascity-serialize-string "scope" scope)
     (gascity-serialize-string "agent" agent))))

;; The pack `[patches]' table holds only `[[patches.agent]]' entries; the
;; city `<gascity-patches-configuration>' is the richer container.
(define-record-type* <gascity-pack-patches-configuration>
  gascity-pack-patches-configuration
  make-gascity-pack-patches-configuration
  gascity-pack-patches-configuration?
  (agents gascity-pack-patches-configuration-agents
          (default 'unset)))

(define (gascity-pack-patches-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-pack-patches-configuration>."
  (match-record config <gascity-pack-patches-configuration> (agents)
    (gascity-serialize-subtables "agent" agents
                                 gascity-agent-patch-configuration->toml)))

(define-record-type* <gascity-pack-global-configuration>
  gascity-pack-global-configuration
  make-gascity-pack-global-configuration
  gascity-pack-global-configuration?
  (session-live gascity-pack-global-configuration-session-live
                (default 'unset)))

(define (gascity-pack-global-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-pack-global-configuration>."
  (match-record config <gascity-pack-global-configuration> (session-live)
    (gascity-serialize-list-of-strings "session_live" session-live)))

(define-record-type* <gascity-pack-runtime-entry-configuration>
  gascity-pack-runtime-entry-configuration
  make-gascity-pack-runtime-entry-configuration
  gascity-pack-runtime-entry-configuration?
  (command         gascity-pack-runtime-entry-configuration-command)  ;string
  (protocol        gascity-pack-runtime-entry-configuration-protocol
                   (default 'unset))
  (prompt-delivery gascity-pack-runtime-entry-configuration-prompt-delivery
                   (default 'unset)))

(define (gascity-pack-runtime-entry-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-pack-runtime-entry-configuration>."
  (match-record config <gascity-pack-runtime-entry-configuration>
    (command protocol prompt-delivery)
    (append
     (gascity-serialize-string-or-gexp "command" command)
     (gascity-serialize-maybe-integer "protocol" protocol)
     (gascity-serialize-maybe-string "prompt_delivery" prompt-delivery))))

(define-record-type* <gascity-pack-doctor-entry-configuration>
  gascity-pack-doctor-entry-configuration
  make-gascity-pack-doctor-entry-configuration
  gascity-pack-doctor-entry-configuration?
  (name        gascity-pack-doctor-entry-configuration-name)   ;string
  (script      gascity-pack-doctor-entry-configuration-script)  ;string
  (description gascity-pack-doctor-entry-configuration-description
               (default 'unset))
  (fix         gascity-pack-doctor-entry-configuration-fix
               (default 'unset))
  (warmup?     gascity-pack-doctor-entry-configuration-warmup?
               (default 'unset)))

(define (gascity-pack-doctor-entry-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-pack-doctor-entry-configuration>."
  (match-record config <gascity-pack-doctor-entry-configuration>
    (name script description fix warmup?)
    (append
     (gascity-serialize-string "name" name)
     (gascity-serialize-string-or-gexp "script" script)
     (gascity-serialize-maybe-string "description" description)
     (gascity-serialize-string-or-gexp "fix" fix)
     (gascity-serialize-maybe-boolean "warmup" warmup?))))

(define-record-type* <gascity-pack-command-entry-configuration>
  gascity-pack-command-entry-configuration
  make-gascity-pack-command-entry-configuration
  gascity-pack-command-entry-configuration?
  (name             gascity-pack-command-entry-configuration-name)  ;string
  (description      gascity-pack-command-entry-configuration-description) ;string
  (long-description gascity-pack-command-entry-configuration-long-description) ;string
  (script           gascity-pack-command-entry-configuration-script)) ;string

(define (gascity-pack-command-entry-configuration->toml config)
  "Return the ordered list of TOML nodes for the body of CONFIG, a
<gascity-pack-command-entry-configuration>."
  (match-record config <gascity-pack-command-entry-configuration>
    (name description long-description script)
    (append
     (gascity-serialize-string "name" name)
     (gascity-serialize-string "description" description)
     (gascity-serialize-string "long_description" long-description)
     (gascity-serialize-string-or-gexp "script" script))))


;;;
;;; Documents.
;;;

(define (gascity-extra-toml-gexp extra-toml)
  "Return EXTRA-TOML as a gexp lowering to its verbatim TOML text: nothing
for #f, the string itself for a string, and the file's contents for a
file-like."
  (cond
   ((eq? extra-toml #f) #~"")
   ((string? extra-toml) extra-toml)
   ((file-like? extra-toml)
    #~(begin
        (use-modules (ice-9 textual-ports))
        (call-with-input-file #$(file-append extra-toml "") get-string-all)))
   (else
    (raise (formatted-message
            (G_ "gascity: extra-toml must be a string, a file-like or #f, got ~s")
            extra-toml)))))

(define (gascity-document->string document extra-toml)
  "Return a gexp lowering to DOCUMENT, a <toml-document>, followed verbatim
by EXTRA-TOML.  A verbatim append can only add whole new tables (see the
escape hatch notes above)."
  #~(string-append #$(toml-document->string document)
                   #$(gascity-extra-toml-gexp extra-toml)))

(define (gascity-supervisor-settings->toml-document config)
  "Return the `supervisor.toml' <toml-document> of CONFIG, a
<gascity-supervisor-settings-configuration>."
  (let ((nodes (gascity-supervisor-settings-configuration->toml config)))
    (gascity-sanitize-and-validate nodes)
    (apply toml-document nodes)))

(define (gascity-supervisor-settings->toml-string config)
  "Return a gexp lowering to the `supervisor.toml' text of CONFIG."
  (gascity-document->string (gascity-supervisor-settings->toml-document config)
                            #f))

(define (gascity-city->toml-document config)
  "Return the `city.toml' <toml-document> of CONFIG, a
<gascity-city-configuration>, with its `extra-config' layered over the
generated nodes."
  (let* ((nodes (gascity-city-configuration->toml config))
         (nodes (gascity-merge-extra-config
                 nodes (gascity-city-configuration-extra-config config))))
    (gascity-sanitize-and-validate nodes)
    (apply toml-document nodes)))

(define (gascity-city->toml-string config)
  "Return a gexp lowering to the `city.toml' text of CONFIG, with its
`extra-toml' appended verbatim."
  (gascity-document->string (gascity-city->toml-document config)
                            (gascity-city-configuration-extra-toml config)))

(define (gascity-pack->toml-document config)
  "Return the `pack.toml' <toml-document> of CONFIG, a
<gascity-pack-configuration>."
  (let ((nodes (gascity-pack-configuration->toml config)))
    (gascity-sanitize-and-validate nodes)
    (apply toml-document nodes)))

(define (gascity-pack->toml-string config)
  "Return a gexp lowering to the `pack.toml' text of CONFIG."
  (gascity-document->string (gascity-pack->toml-document config) #f))


;;;
;;; System service type and lifecycle.
;;;
;;; The value of `gascity-service-type' is a LIST of
;;; <gascity-supervisor-configuration> records, one per machine-wide
;;; supervisor process.  Gas City isolates supervisors by `GC_HOME' (the
;;; registry, settings, control socket and instance lock all live under it),
;;; so the service rejects two instances that would share a `gc-home', and
;;; two that would bind the same `(bind, port)'.
;;;
;;; Each instance contributes a long-running Shepherd service (the
;;; secrets-loading wrapper that execs `gc supervisor run') and a paired
;;; provision one-shot it requires.  The provision one-shot runs genesis,
;;; pack fetching and the registry merge as the service user, because Guix
;;; activation scripts run as root and cannot switch user (§16.1).

(define %gascity-user
  ;; The account every instance runs as, unless it overrides `user'.
  "gascity")

(define %gascity-group
  "gascity")

(define %gascity-state-directory
  ;; The account's home, and the parent of the derived city directories.
  "/var/lib/gascity")

(define %gascity-log-directory
  "/var/log/gascity")

(define %gascity-default-port
  ;; The API port a lone instance binds; Gas City resolves any `<= 0' value to
  ;; it, so it is never "disabled" and a second instance must set its own.
  8372)

(define %gascity-default-dolt-user-name
  ;; Dolt requires a commit author identity, and `gc init' of a managed bd
  ;; store fails at genesis without one.  This is the identity a pristine
  ;; service falls back to when the operator sets neither `dolt-user-name'
  ;; nor `dolt-user-email' and no identity file exists yet.
  "Gas City")

(define %gascity-default-dolt-user-email
  "gascity@localhost")

(define (gascity-supervisor-user config)
  "Return the user name the supervisor instance CONFIG runs as."
  (let ((user (gascity-supervisor-configuration-user config)))
    (if (eq? user 'unset) %gascity-user user)))

(define (gascity-supervisor-group config)
  "Return the group name the supervisor instance CONFIG runs as."
  (let ((group (gascity-supervisor-configuration-group config)))
    (if (eq? group 'unset) %gascity-group group)))

(define (gascity-supervisor-state-directory config)
  "Return the state directory of the supervisor instance CONFIG, the home of
its account and the parent of its derived city directories."
  (let ((directory (gascity-supervisor-configuration-state-directory config)))
    (if (eq? directory 'unset) %gascity-state-directory directory)))

(define (gascity-supervisor-log-directory config)
  "Return the log directory of the supervisor instance CONFIG."
  (let ((directory (gascity-supervisor-configuration-log-directory config)))
    (if (eq? directory 'unset) %gascity-log-directory directory)))

(define (gascity-supervisor-gc-home config)
  "Return the GC_HOME of the supervisor instance CONFIG: its `gc-home' field,
`<state-directory>/.gc' otherwise.  The registry, settings, control socket
and instance lock all live under it."
  (let ((home (gascity-supervisor-configuration-gc-home config)))
    (if (eq? home 'unset)
        (string-append (gascity-supervisor-state-directory config) "/.gc")
        home)))

(define (gascity-supervisor-port config)
  "Return the API port the supervisor instance CONFIG binds (an integer), or
`unset': its `port' field, falling back to the port of its settings record."
  (let ((port (gascity-supervisor-configuration-port config)))
    (if (eq? port 'unset)
        (let ((settings (gascity-supervisor-configuration-settings config)))
          (if (eq? settings 'unset)
              'unset
              (gascity-supervisor-settings-configuration-port settings)))
        port)))

(define (gascity-supervisor-secrets-file config)
  "Return the dotenv file the supervisor instance CONFIG loads into its
environment, `<gc-home>/secrets.env' by default.  The file holds the values
of the `$VAR' references of the generated TOML; it is never part of the
service gexp (§16.17)."
  (let ((file (gascity-supervisor-configuration-secrets-file config)))
    (if (eq? file 'unset)
        (string-append (gascity-supervisor-gc-home config) "/secrets.env")
        file)))

(define (gascity-supervisor-dolt-user-name config)
  "Return the effective Dolt `user.name' of the supervisor instance CONFIG:
its `dolt-user-name' field, or the default identity when unset."
  (let ((name (gascity-supervisor-configuration-dolt-user-name config)))
    (if (eq? name 'unset) %gascity-default-dolt-user-name name)))

(define (gascity-supervisor-dolt-user-email config)
  "Return the effective Dolt `user.email' of the supervisor instance CONFIG:
its `dolt-user-email' field, or the default identity when unset."
  (let ((email (gascity-supervisor-configuration-dolt-user-email config)))
    (if (eq? email 'unset) %gascity-default-dolt-user-email email)))

(define (gascity-supervisor-dolt-identity-explicit? config)
  "Return #t when the operator set the supervisor instance CONFIG's
`dolt-user-name' or `dolt-user-email' field, i.e. declared a Dolt author
identity explicitly."
  (or (not (eq? (gascity-supervisor-configuration-dolt-user-name config)
                'unset))
      (not (eq? (gascity-supervisor-configuration-dolt-user-email config)
                'unset))))

(define (gascity-json-string value)
  "Return VALUE, a string, quoted and escaped as a JSON string."
  (define (hex character)
    (string-pad (number->string (char->integer character) 16) 4 #\0))
  (string-append
   "\""
   (call-with-output-string
    (lambda (port)
      (string-for-each
       (lambda (character)
         (cond
          ((char=? character #\") (display "\\\"" port))
          ((char=? character #\\) (display "\\\\" port))
          ((char=? character #\newline) (display "\\n" port))
          ((char=? character #\return) (display "\\r" port))
          ((char=? character #\tab) (display "\\t" port))
          ((char<? character #\space)
           (display "\\u" port)
           (display (hex character) port))
          (else (display character port))))
       value)))
   "\""))

(define (gascity-dolt-config-global-json name email)
  "Return the text of a Dolt global configuration file (`config_global.json')
that declares the commit author identity NAME and EMAIL.  Dolt reads it from
`$DOLT_ROOT_PATH/.dolt' (or `$HOME/.dolt'), which the service pins to the
supervisor's state directory."
  (string-append "{\n"
                 "  \"user.name\": " (gascity-json-string name) ",\n"
                 "  \"user.email\": " (gascity-json-string email) "\n"
                 "}\n"))

(define (gascity-supervisor-dolt-config-file config)
  "Return the path of the Dolt global configuration file of the supervisor
instance CONFIG: `<state-directory>/.dolt/config_global.json'.  Dolt reads it
through `$DOLT_ROOT_PATH'/`$HOME', both pinned to the state directory."
  (string-append (gascity-supervisor-state-directory config)
                 "/.dolt/config_global.json"))

(define (gascity-supervisor-dolt-config-global config installed?)
  "Return the JSON text the activation must install as the supervisor instance
CONFIG's `<state-directory>/.dolt/config_global.json', or #f when it must
leave the existing file untouched.

An explicit operator identity (`dolt-user-name' or `dolt-user-email', either
one) is intent, so it is written even when INSTALLED? is true, the default
identity filling whichever of the two is unset.  When both are unset the
default identity is written only when no file exists yet: an existing file is
an operator- or sops-provided identity and wins (no clobber)."
  (if (and installed? (not (gascity-supervisor-dolt-identity-explicit? config)))
      #f
      (gascity-dolt-config-global-json
       (gascity-supervisor-dolt-user-name config)
       (gascity-supervisor-dolt-user-email config))))

(define (gascity-supervisor-log-file config)
  "Return the log file of the supervisor instance CONFIG, named after its id."
  (let ((id (gascity-supervisor-configuration-id config)))
    (string-append (gascity-supervisor-log-directory config)
                   "/supervisor"
                   (if (and (string? id) (not (string-null? id)))
                       (string-append "-" id)
                       "")
                   ".log")))

(define (gascity-city-directory config city)
  "Return the directory of CITY under the supervisor instance CONFIG: its
`directory' field, `<state-directory>/<name>' otherwise."
  (let ((directory (gascity-city-configuration-directory city)))
    (if (eq? directory 'unset)
        (string-append (gascity-supervisor-state-directory config)
                       "/" (gascity-city-configuration-name city))
        directory)))

(define (gascity-supervisor-provision config)
  "Return the Shepherd service name of the provision one-shot of the
supervisor instance CONFIG: `gascity-provision', suffixed with its id."
  (let ((id (gascity-supervisor-configuration-id config)))
    (if (and (string? id) (not (string-null? id)))
        (string->symbol (string-append "gascity-provision-" id))
        'gascity-provision)))

(define (gascity-supervisor-shepherd-service-name config)
  "Return the Shepherd service name of the supervisor instance CONFIG:
`gascity-supervisor', suffixed with its id."
  (let ((id (gascity-supervisor-configuration-id config)))
    (if (and (string? id) (not (string-null? id)))
        (string->symbol (string-append "gascity-supervisor-" id))
        'gascity-supervisor)))

(define (gascity-supervisor-cities config)
  "Return the enabled cities of the supervisor instance CONFIG."
  (filter gascity-city-configuration-enabled?
          (gascity-supervisor-configuration-cities config)))


;;;
;;; Validation and normalization.
;;;

(define %gascity-id-char-set
  ;; Characters allowed in a supervisor id: it becomes a Shepherd service
  ;; name, part of the log file name and part of the gc-home socket path.
  (char-set-intersection char-set:ascii
                         (char-set-union char-set:letter+digit
                                         (string->char-set "._-"))))

(define (gascity-valid-id? id)
  (and (string? id)
       (not (string-null? id))
       (string-every %gascity-id-char-set id)))

(define (gascity-find-duplicates items)
  "Return the elements of ITEMS that appear more than once, each once, in
order of first repetition."
  (let loop ((items items) (seen '()) (duplicates '()))
    (if (null? items)
        (reverse duplicates)
        (let ((head (car items)))
          (loop (cdr items)
                (cons head seen)
                (if (and (member head seen) (not (member head duplicates)))
                    (cons head duplicates)
                    duplicates))))))

(define (gascity-supervisor-configurations value)
  "Return the normalized list of <gascity-supervisor-configuration> records
the service VALUE stands for, validating it and filling in derived values.

Raise a `formatted-message' when VALUE is not a list of records, when an id
is invalid or two instances would provide the same Shepherd service, when a
multi-instance service leaves a `port' implicit, or when two instances share
a `gc-home' or a `(bind, port)' (each would fight over the same socket, lock
or listening address).  A lone instance defaults to port 8372; with two or
more instances, every instance must set `port' explicitly (§4.1, §13.2)."
  (unless (and (list? value)
               (every gascity-supervisor-configuration? value))
    (raise (formatted-message
            (G_ "gascity: the service value must be a list of \
<gascity-supervisor-configuration> records, got ~s")
            value)))

  (define configs
    (map (lambda (config)
           (let ((id (gascity-supervisor-configuration-id config)))
             (unless (or (eq? id 'unset) (eq? id #f) (gascity-valid-id? id))
               (raise (formatted-message
                       (G_ "gascity: invalid supervisor id ~s (expected a \
non-empty string of letters, digits, '.', '_', and '-')")
                       id)))
             (gascity-supervisor-configuration
              (inherit config)
              (id (if (or (eq? id 'unset) (eq? id #f)) 'unset id)))))
         value))

  (define normalized
    (if (= (length configs) 1)
        (let ((config (car configs)))
          (if (eq? (gascity-supervisor-port config) 'unset)
              (list (gascity-supervisor-configuration
                     (inherit config) (port %gascity-default-port)))
              configs))
        (begin
          (for-each
           (lambda (config)
             (when (eq? (gascity-supervisor-port config) 'unset)
               (raise (formatted-message
                       (G_ "gascity: every supervisor instance must set \
'port' explicitly when the service has more than one; ~a does not")
                       (gascity-supervisor-provision config)))))
           configs)
          configs)))

  (let ((duplicates (gascity-find-duplicates
                     (map gascity-supervisor-provision normalized))))
    (unless (null? duplicates)
      (raise (formatted-message
              (G_ "gascity: more than one supervisor instance would provide \
the Shepherd service '~a'; give each instance a distinct 'id'")
              (car duplicates)))))

  (let ((duplicates (gascity-find-duplicates
                     (map gascity-supervisor-gc-home normalized))))
    (unless (null? duplicates)
      (raise (formatted-message
              (G_ "gascity: more than one supervisor instance uses 'gc-home' \
~s; each instance needs its own gc-home so it has its own socket and lock")
              (car duplicates)))))

  (let ((duplicates (gascity-find-duplicates
                     (map (lambda (config)
                            (cons (gascity-supervisor-configuration-bind config)
                                  (gascity-supervisor-port config)))
                          normalized))))
    (unless (null? duplicates)
      (raise (formatted-message
              (G_ "gascity: more than one supervisor instance binds \
~a:~a; each instance needs a distinct (bind, port)")
              (car (car duplicates)) (cdr (car duplicates))))))

  normalized)

(define-syntax-rule (gascity-service-config field ...)
  "Return a service value holding a single
<gascity-supervisor-configuration>, so the common one-instance case is not a
one-element list."
  (list (gascity-supervisor-configuration field ...)))


;;;
;;; `cities.toml' read-merge-write.
;;;
;;; The registry holds `[[cities]]', `[[rigs]]' and `[[pending_city_requests]]'
;;; in one file, and `gc' rewrites it under an flock.  Guix owns exactly the
;;; `[[cities]]' set (the declared and enabled cities); everything else is
;;; preserved verbatim, so a reconfigure never clobbers a rig registration or
;;; an in-flight request (§16.3).
;;;
;;; The transform is pure and depends only on Guile core plus SRFI-1/13, so
;;; the very same code runs both at the top level (and in the unit tests) and
;;; inside the provision program (the same forms are evaluated there, see
;;; `gascity-cities-toml-merge-forms' below).

(define gascity-cities-toml-merge-forms
  ;; The pure transforms are kept as data and evaluated both here (for the
  ;; unit tests and the service code) and inside the provision program (which
  ;; cannot import this module, as it pulls in the whole Guix package graph).
  '((define (gascity-toml-table-header line)
      "Return (KIND . KEY) when LINE is a table header, #f otherwise.  KIND is
`table' for `[KEY]' and `array' for `[[KEY]]'."
      (let ((line (string-trim-both line)))
        (cond
         ((string-prefix? "[[" line)
          (let ((end (string-index line #\])))
            (and end (> end 2)
                 (cons 'array (string-trim-both (substring line 2 end))))))
         ((string-prefix? "[" line)
          (let ((end (string-index line #\])))
            (and end (> end 1)
                 (cons 'table (string-trim-both (substring line 1 end))))))
         (else #f))))

    (define (gascity-toml-sections text)
      "Return the sections of TEXT as a list of (HEADER . LINES): each header
is (KIND . KEY) or #f for the preamble, and LINES are the raw lines of the
section, in order."
      (let loop ((lines (string-split (or text "") #\newline))
                 (header #f)
                 (current '())
                 (sections '()))
        (if (null? lines)
            (reverse
             (if (or header (pair? current))
                 (cons (cons header (reverse current)) sections)
                 sections))
            (let* ((line (car lines))
                   (found (gascity-toml-table-header line)))
              (if found
                  (loop (cdr lines) found (list line)
                        (if (pair? current)
                            (cons (cons header (reverse current)) sections)
                            sections))
                  (loop (cdr lines) header (cons line current) sections))))))

    (define (gascity-toml-quote-string string)
      "Return STRING as a TOML basic string literal, quotes included."
      (call-with-output-string
       (lambda (port)
         (write-char #\" port)
         (string-for-each
          (lambda (character)
            (case character
              ((#\") (display "\\\"" port))
              ((#\\) (display "\\\\" port))
              ((#\newline) (display "\\n" port))
              ((#\return) (display "\\r" port))
              ((#\tab) (display "\\t" port))
              (else (write-char character port))))
          string)
         (write-char #\" port))))

    (define (gascity-cities->toml-lines cities)
      "Return the lines of the `[[cities]]' blocks for CITIES, a list of
(PATH . NAME) pairs, NAME being a string or `unset'."
      (append-map
       (lambda (city)
         (let ((path (car city))
               (name (cdr city)))
           (append
            (list "[[cities]]"
                  (string-append "path = " (gascity-toml-quote-string path)))
            (if (and (string? name) (not (string-null? name)))
                (list (string-append "name = "
                                     (gascity-toml-quote-string name)))
                '()))))
       cities))

    (define (gascity-strip-leading-blank-lines lines)
      "Return LINES without the blank lines that precede the first non-blank
line."
      (cond ((null? lines) '())
            ((string-null? (car lines))
             (gascity-strip-leading-blank-lines (cdr lines)))
            (else lines)))

    (define (gascity-trim-blank-lines lines)
      "Return LINES without the blank lines that precede the first and follow
the last non-blank line."
      (reverse
       (gascity-strip-leading-blank-lines
        (reverse (gascity-strip-leading-blank-lines lines)))))

    (define (gascity-cities-toml-merge existing cities)
      "Return new `cities.toml' text: replace the `[[cities]]' set of EXISTING
(a string, or #f for an empty file) with CITIES while preserving the raw text
of every other section, in particular `[[rigs]]' and
`[[pending_city_requests]]'."
      (let* ((sections (gascity-toml-sections (or existing "")))
             (kept (filter (lambda (section)
                             (not (equal? (car section)
                                          (cons 'array "cities"))))
                           sections))
             (body (gascity-trim-blank-lines (append-map cdr kept)))
             (cities-lines (gascity-cities->toml-lines cities)))
        (cond
         ((and (null? body) (null? cities-lines)) "")
         ((null? cities-lines)
          (string-append (string-join body "\n") "\n"))
         (else
          (string-append
           (string-join (if (null? body)
                            cities-lines
                            (append body (list "") cities-lines))
                        "\n")
           "\n")))))))

(for-each (lambda (form)
            (eval form (current-module)))
          gascity-cities-toml-merge-forms)


;;;
;;; `.gc/site.toml'.
;;;
;;; Site bindings moved out of `city.toml' in schema 2: the machine-local
;;; workspace identity and rig paths live in `.gc/site.toml'.

(define (gascity-rig-site->toml rig)
  "Return the TOML nodes of RIG, a <gascity-rig-configuration>, as a
`.gc/site.toml' `[[rig]]' element."
  (append
   (gascity-serialize-string "name" (gascity-rig-configuration-name rig))
   (gascity-serialize-maybe-string "path" (gascity-rig-configuration-path rig))))

(define (gascity-city-site->toml-string city)
  "Return a gexp lowering to the `.gc/site.toml' text of CITY, a
<gascity-city-configuration>: its workspace name and prefix and one `[[rig]]'
entry per declared rig (name and path)."
  (let* ((workspace (gascity-city-configuration-workspace city))
         (nodes
          (append
           (if (eq? workspace 'unset)
               '()
               (append
                (gascity-serialize-maybe-string
                 "workspace_name"
                 (gascity-workspace-configuration-name workspace))
                (gascity-serialize-maybe-string
                 "workspace_prefix"
                 (gascity-workspace-configuration-prefix workspace))))
           (gascity-serialize-subtables
            "rig" (gascity-city-configuration-rigs city) gascity-rig-site->toml))))
    (gascity-document->string (apply toml-document nodes) #f)))


;;;
;;; supervisor.toml.
;;;

(define (gascity-supervisor-effective-settings config)
  "Return the settings record whose `supervisor.toml' text the supervisor
instance CONFIG owns: its `settings' field, with the listener `bind' and
`port' of CONFIG layered over it (those fields are the authoritative ones)."
  (let* ((settings (gascity-supervisor-configuration-settings config))
         (settings (if (eq? settings 'unset)
                       (gascity-supervisor-settings-configuration)
                       settings))
         (port (gascity-supervisor-port config))
         (bind (gascity-supervisor-configuration-bind config)))
    (gascity-supervisor-settings-configuration
     (inherit settings)
     (port (if (eq? port 'unset)
               (gascity-supervisor-settings-configuration-port settings)
               port))
     (bind (if (eq? bind 'unset)
               (gascity-supervisor-settings-configuration-bind settings)
               bind)))))

(define (gascity-supervisor-toml-string config)
  "Return a gexp lowering to the `supervisor.toml' text of the supervisor
instance CONFIG."
  (gascity-supervisor-settings->toml-string
   (gascity-supervisor-effective-settings config)))


;;;
;;; PATH.
;;;

(define (gascity-input-package input)
  "Return the package INPUT stands for, or #f when INPUT is not a package
input (transitive inputs are (LABEL PACKAGE OUTPUTS...) lists)."
  (cond ((package? input) input)
        ((and (pair? input) (package? (cadr input))) (cadr input))
        ((and (pair? input) (package? (car input))) (car input))
        (else #f)))

(define (gascity-supervisor-path-packages config)
  "Return the packages whose bin directories head the PATH of the supervisor
instance CONFIG: its `package' and the closure of the packages it propagates
(so every tool `gc' looks up by name is present), its `packages' field, plus
coreutils, git and the helpers the bundled beads/dolt lifecycle script
(`gc-beads-bd.sh') invokes by name.  The provision program *replaces* PATH
with this list, so without those helpers the system's own `sed', `awk', etc.
would be invisible and genesis would fail (§2.5)."
  (let ((package (gascity-supervisor-configuration-package config)))
    (delete-duplicates
     (append
      (list package)
      (filter-map gascity-input-package
                  (package-transitive-propagated-inputs package))
      (gascity-supervisor-configuration-packages config)
      (list coreutils git sed gawk which bash sqlite netcat)))))

(define (gascity-supervisor-environment config)
  "Return a gexp lowering to the Shepherd environment-variables list of the
supervisor instance CONFIG: HOME, DOLT_ROOT_PATH (pinned to the state
directory, the account's home, so Dolt reads the activation-materialized
`<state-directory>/.dolt/config_global.json' author identity), GC_HOME,
XDG_RUNTIME_DIR (pinned to `gc-home' so an isolated supervisor never touches
the host socket, §16.2), GC_SUPERVISOR_PRESERVE_SESSIONS_ON_SIGNAL, PATH, and
the operator's non-secret `environment-variables'."
  (let ((home (gascity-supervisor-state-directory config))
        (gc-home (gascity-supervisor-gc-home config))
        (packages (gascity-supervisor-path-packages config))
        (extra (gascity-supervisor-configuration-environment-variables config)))
    #~(list (string-append "HOME=" #$home)
            (string-append "DOLT_ROOT_PATH=" #$home)
            (string-append "GC_HOME=" #$gc-home)
            (string-append "XDG_RUNTIME_DIR=" #$gc-home)
            "GC_SUPERVISOR_PRESERVE_SESSIONS_ON_SIGNAL=1"
            (string-append "PATH="
                           (string-join
                            (list #$@(map (lambda (package)
                                            (file-append package "/bin"))
                                          packages))
                            ":"))
            #$@extra)))


;;;
;;; Programs.
;;;

(define* (gascity-supervisor-activation-program config #:key (chown? #t))
  "Return a Guile program, as a file-like object, that materializes the
files of the supervisor instance CONFIG: it creates the state, log and
gc-home directories 0700 and materializes each enabled city's generated
`city.toml', `pack.toml' and `files'/`rig-files' (preserving executable
bits) and each instance's `supervisor.toml'.  Unchanged generated files are
left alone, so a reconfigure that changes nothing does not perturb a running
controller.  It never runs `gc init' (that is the provision one-shot's job).

When CHOWN? is true (the default) it also chowns everything to the service
user and group, which is what system activation needs (it runs as root and
cannot switch user, §16.1).  The Home activation passes CHOWN? #f: it runs as
the user that owns the Home environment, so the files are already theirs."
  (let ((user (gascity-supervisor-user config))
        (group (gascity-supervisor-group config))
        (state-directory (gascity-supervisor-state-directory config))
        (log-directory (gascity-supervisor-log-directory config))
        (gc-home (gascity-supervisor-gc-home config))
        (supervisor-toml (gascity-supervisor-toml-string config))
        (dolt-config-file (gascity-supervisor-dolt-config-file config))
        (dolt-config (gascity-supervisor-dolt-config-global config #f))
        (dolt-identity-explicit?
         (gascity-supervisor-dolt-identity-explicit? config))
        (cities (gascity-supervisor-cities config)))
    (program-file
     "gascity-activation"
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 textual-ports))

           (define user #$user)
           (define group #$group)

           (define (owner)
             ;; Resolve the ids at run time: the account exists by the time
             ;; activation runs.
             (let ((account (getpwnam user))
                   (grp (getgrnam group)))
               (cons (if account (passwd:uid account) 0)
                     (if grp (group:gid grp) 0))))

           (define (own file)
             #$(if chown?
                   #~(let ((ids (owner)))
                       (chown file (car ids) (cdr ids)))
                   #~#t))

           (define (ensure-directory directory)
             (mkdir-p directory)
             (chmod directory #o700)
             (own directory))

           (define (install-file file content mode)
             (let ((same? (and (file-exists? file)
                               (let ((old (call-with-input-file file
                                            get-string-all)))
                                 (string=? old content)))))
               (unless same?
                 (call-with-output-file file
                   (lambda (port) (display content port))))
               (chmod file mode)
               (own file)))

           (define (install-static-file file source)
             ;; copy-file preserves the executable bit of scripts.
             (mkdir-p (dirname file))
             (copy-file source file)
             (own file))

           (ensure-directory #$state-directory)
           (ensure-directory #$log-directory)
           (ensure-directory #$gc-home)

           (install-file (string-append #$gc-home "/supervisor.toml")
                         #$supervisor-toml #o600)

           ;; Dolt requires a commit author identity, and `gc init' of a
           ;; managed bd store fails at genesis without one.  It reads
           ;; `$HOME/.dolt/config_global.json' (the service user's home is
           ;; the state directory).  The same decision as
           ;; `gascity-supervisor-dolt-config-global': an explicit operator
           ;; identity always wins, while the default identity is written
           ;; only when no file exists yet, so an operator- or
           ;; sops-provided file is never clobbered.
           (let ((dolt-config-file #$dolt-config-file))
             (when (or #$dolt-identity-explicit?
                       (not (file-exists? dolt-config-file)))
               ;; Own the `.dolt' directory (not just the config file): Dolt
               ;; writes its event/telemetry data to `<state>/.dolt/eventsData'
               ;; when `gc init' runs as the service user, so a root-owned
               ;; directory would make genesis fail with EACCES.
               (ensure-directory (dirname dolt-config-file))
               (install-file dolt-config-file #$dolt-config #o600)))

           #$@(append-map
               (lambda (city)
                 (let* ((directory (gascity-city-directory config city))
                        (city-toml (gascity-city->toml-string city))
                        (pack (gascity-city-configuration-pack city))
                        (files (gascity-city-configuration-files city))
                        (rig-files (gascity-city-configuration-rig-files city))
                        (rigs (gascity-city-configuration-rigs city))
                        (rig-directories
                         (filter-map
                          (lambda (rig)
                            (let ((path (gascity-rig-configuration-path rig)))
                              (and (not (eq? path 'unset)) path)))
                          (if (eq? rigs 'unset) '() rigs))))
                   (append
                    (list
                     #~(begin
                         (ensure-directory #$directory)
                         (install-file (string-append #$directory "/city.toml")
                                       #$city-toml #o600)
                         #$@(if (eq? pack 'unset)
                                '()
                                (list
                                 #~(install-file
                                    (string-append #$directory "/pack.toml")
                                    #$(gascity-pack->toml-string pack)
                                    #o600)))))
                    (map (lambda (entry)
                           #~(install-static-file
                              (string-append #$directory "/" #$(car entry))
                              #$(cdr entry)))
                         (if (list? files) files '()))
                    (append-map
                     (lambda (rig-directory)
                       (map (lambda (entry)
                              #~(install-static-file
                                 (string-append #$rig-directory "/" #$(car entry))
                                 #$(cdr entry)))
                            (if (list? rig-files) rig-files '())))
                     rig-directories))))
               cities))))))

(define (gascity-supervisor-provision-program config)
  "Return a Guile program, as a file-like object, that provisions the
supervisor instance CONFIG as the service user: for each enabled city it
writes `.gc/site.toml', runs the idempotent `gc init --file ...
--preserve-existing --no-start --skip-provider-readiness', optionally runs
`gc import install' (`install-packs?'), and then flock-guarded
read-merge-writes `<gc-home>/cities.toml', reloading a running supervisor
when the registry changed.  It performs no network access unless a city opts
into `install-packs?'."
  (let ((user (gascity-supervisor-user config))
        (state-directory (gascity-supervisor-state-directory config))
        (gc-home (gascity-supervisor-gc-home config))
        (gc (file-append (gascity-supervisor-configuration-package config)
                         "/bin/gc"))
        (packages (gascity-supervisor-path-packages config))
        (cities (gascity-supervisor-cities config)))
    (program-file
     "gascity-provision"
     (with-imported-modules '((guix build utils)
                              (guix build syscalls))
       #~(begin
           (use-modules (guix build utils)
                        (guix build syscalls)
                        (ice-9 textual-ports)
                        (srfi srfi-1)
                        (srfi srfi-13))

           ;; Define the registry merge in this program's module: the pure
           ;; forms are shared with the top level (see
           ;; `gascity-cities-toml-merge-forms').  This module cannot import
           ;; (r0man guix services gascity), which pulls in the whole Guix
           ;; package graph.
           (for-each (lambda (form)
                       (eval form (current-module)))
                     '#$gascity-cities-toml-merge-forms)

           ;; The one-shot redirects its own diagnostics to the provision log
           ;; (the log directory is owned by the service user): shepherd
           ;; 0.10.x's `spawn-command', which the paired one-shot service uses
           ;; so the supervisor only starts after genesis, has no `#:log-file'
           ;; keyword.
           (catch #t
             (lambda ()
               (let ((log-file #$(gascity-supervisor-provision-log-file config)))
                 (mkdir-p (dirname log-file))
                 (let ((port (open-file log-file "a")))
                   (setvbuf port 'line)
                   (set-current-error-port port)
                   (set-current-output-port port))))
             (lambda args #t))

           (define user #$user)

           (define (log fmt . args)
             (apply format (current-error-port)
                    (string-append "gascity[" user "]: " fmt "~%")
                    args))

           (define (fail fmt . args)
             (apply log fmt args)
             (exit 1))

           (define (install-file file content mode)
             (let ((same? (and (file-exists? file)
                               (let ((old (call-with-input-file file
                                            get-string-all)))
                                 (string=? old content)))))
               (unless same?
                 (call-with-output-file file
                   (lambda (port) (display content port))))
               (chmod file mode)))

           (setenv "HOME" #$state-directory)
           ;; Pin Dolt's global configuration root to the account's home (the
           ;; state directory) so `gc init' reads the author identity the
           ;; activation materialized as `<state>/.dolt/config_global.json'.
           (setenv "DOLT_ROOT_PATH" #$state-directory)
           (setenv "GC_HOME" #$gc-home)
           (setenv "XDG_RUNTIME_DIR" #$gc-home)
           (setenv "PATH"
                   (string-join
                    (list #$@(map (lambda (package)
                                    (file-append package "/bin"))
                                  packages))
                    ":"))
           (when (and (not (getenv "SSL_CERT_DIR"))
                      (file-exists? "/etc/ssl/certs"))
             (setenv "SSL_CERT_DIR" "/etc/ssl/certs"))
           (when (and (not (getenv "SSL_CERT_FILE"))
                      (file-exists? "/etc/ssl/certs/ca-certificates.crt"))
             (setenv "SSL_CERT_FILE" "/etc/ssl/certs/ca-certificates.crt"))
           (when (and (not (getenv "GUIX_LOCPATH"))
                      (file-exists? "/run/current-system/locale"))
             (setenv "GUIX_LOCPATH" "/run/current-system/locale"))

           (define city-entries
             (list #$@(map (lambda (city)
                             #~(list #$(gascity-city-directory config city)
                                     #$(gascity-city-site->toml-string city)
                                     #$(gascity-city-configuration-genesis? city)
                                     #$(gascity-city-configuration-install-packs? city)
                                     #$(gascity-city-configuration-register? city)
                                     #$(gascity-city-configuration-name city)))
                           cities)))

           (for-each
            (lambda (entry)
              (let ((directory (list-ref entry 0))
                    (site (list-ref entry 1))
                    (genesis? (list-ref entry 2))
                    (install-packs? (list-ref entry 3))
                    (name (list-ref entry 5)))
                (log "provisioning city ~a in ~a" name directory)
                (mkdir-p (string-append directory "/.gc"))
                (chmod (string-append directory "/.gc") #o700)
                (install-file (string-append directory "/.gc/site.toml")
                              site #o600)
                (when genesis?
                  (unless (zero?
                           (system* #$gc "init"
                                    "--file"
                                    (string-append directory "/city.toml")
                                    "--preserve-existing"
                                    "--no-start"
                                    "--skip-provider-readiness"
                                    directory))
                    (fail "gc init failed for city ~a" name)))
                (when install-packs?
                  (unless (zero? (system* #$gc "import" "install"
                                          "--city" directory))
                    (fail "gc import install failed for city ~a" name)))))
            city-entries)

           (define registry (string-append #$gc-home "/cities.toml"))

           (define registry-entries
             (filter-map (lambda (entry)
                           (and (list-ref entry 4)
                                (cons (list-ref entry 0) (list-ref entry 5))))
                         city-entries))

           (let* ((original (if (file-exists? registry)
                                (call-with-input-file registry get-string-all)
                                ""))
                  (desired (gascity-cities-toml-merge original
                                                      registry-entries)))
             (unless (string=? original desired)
               (let ((lock-port (open-file (string-append registry ".lock") "a")))
                 (chmod (string-append registry ".lock") #o600)
                 (flock (fileno lock-port) LOCK_EX)
                 (let* ((current (if (file-exists? registry)
                                     (call-with-input-file registry
                                       get-string-all)
                                     ""))
                        (merged (gascity-cities-toml-merge current
                                                           registry-entries)))
                   (unless (string=? current merged)
                     (let ((temporary (string-append registry ".tmp")))
                       (call-with-output-file temporary
                         (lambda (port) (display merged port)))
                       (chmod temporary #o600)
                       (rename-file temporary registry))))
                 (flock (fileno lock-port) LOCK_UN)
                 (close-port lock-port))
               ;; Best-effort, non-fatal: only a running supervisor reloads.
               (when (any file-exists?
                          (list (string-append #$gc-home "/gc/supervisor.sock")
                                (string-append #$gc-home "/supervisor.sock")))
                 (false-if-exception
                  (system* #$gc "supervisor" "reload")))))

           (log "provisioning complete"))))))

(define (gascity-supervisor-program config)
  "Return a Guile program, as a file-like object, that loads the dotenv
`secrets-file' of the supervisor instance CONFIG into its environment (mode
0600, owned by the service user, or a decrypted path such as a sops-guix
output) and then execs `gc supervisor run'.  No secret is embedded in the
service gexp: only the file's keys reach the process environment (§16.17)."
  (let ((package (gascity-supervisor-configuration-package config))
        (gc-home (gascity-supervisor-gc-home config))
        (secrets-file (gascity-supervisor-secrets-file config)))
    (program-file
     "gascity-supervisor"
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 textual-ports)
                        (srfi srfi-1)
                        (srfi srfi-13))

           (define (log fmt . args)
             (apply format (current-error-port)
                    (string-append "gascity-supervisor: " fmt "~%")
                    args))

           (define secrets-file #$secrets-file)
           (define default-secrets-file #$(string-append gc-home "/secrets.env"))

           (define (env-identifier? key)
             (and (not (string-null? key))
                  (let ((first (string-ref key 0)))
                    (and (or (char-alphabetic? first) (char=? first #\_))
                         (string-every
                          (lambda (character)
                            (or (char-alphabetic? character)
                                (char-numeric? character)
                                (char=? character #\_)))
                          key)))))

           (define (unquote-value value)
             (if (and (>= (string-length value) 2)
                      (or (and (char=? (string-ref value 0) #\")
                               (char=? (string-ref value
                                                   (- (string-length value) 1))
                                       #\"))
                          (and (char=? (string-ref value 0) #\')
                               (char=? (string-ref value
                                                   (- (string-length value) 1))
                                       #\'))))
                 (substring value 1 (- (string-length value) 1))
                 value))

           (define (parse-line line)
             ;; A single-line subset of Gas City's processenv.ParseEnvFile:
             ;; KEY=VALUE per line, '#' comments, an optional leading "export",
             ;; and one layer of matching surrounding quotes.
             (let ((line (string-trim-both line)))
               (cond
                ((or (string-null? line) (char=? (string-ref line 0) #\#)) #f)
                (else
                 (let* ((line (if (string-prefix? "export " line)
                                  (substring line 7)
                                  line))
                        (index (string-index line #\=)))
                   (and index
                        (let ((key (string-trim-both (substring line 0 index)))
                              (value (string-trim-both
                                      (substring line (1+ index)))))
                          (and (env-identifier? key)
                               (cons key (unquote-value value))))))))))

           (define (load-secrets file)
             (let loop ((lines (string-split
                                (call-with-input-file file get-string-all)
                                #\newline)))
               (unless (null? lines)
                 (let ((parsed (parse-line (car lines))))
                   (when parsed
                     (setenv (car parsed) (cdr parsed))))
                 (loop (cdr lines)))))

           (cond
            ((file-exists? secrets-file)
             (log "loading secrets from ~a" secrets-file)
             (load-secrets secrets-file))
            ((string=? secrets-file default-secrets-file)
             ;; Materialize the default secrets file 0600, owned by the user
             ;; (this program runs as the service user).  A $VAR reference
             ;; then resolves to an empty value instead of failing.  An
             ;; override, such as a decrypted sops file, is left to its
             ;; provider.
             (log "creating empty secrets file ~a" default-secrets-file)
             (mkdir-p (dirname default-secrets-file))
             (call-with-output-file default-secrets-file (lambda (port) #t))
             (chmod default-secrets-file #o600))
            (else
             (log "secrets file ~a does not exist; continuing" secrets-file)))

           ;; GC_SUPERVISOR_ENV is inherited from the service environment and
           ;; left untouched: it is the allowlist `gc' consults when it
           ;; forwards non-GC_ variables to spawned agent sessions, so a
           ;; secret reaches agents only when named there (§16.17).
           (execl #$(file-append package "/bin/gc") "gc" "supervisor" "run"))))))


;;;
;;; Shepherd services.
;;;

(define (gascity-supervisor-provision-log-file config)
  "Return the log file of the provision one-shot of the supervisor instance
CONFIG."
  (let ((id (gascity-supervisor-configuration-id config)))
    (string-append (gascity-supervisor-log-directory config)
                   "/supervisor"
                   (if (and (string? id) (not (string-null? id)))
                       (string-append "-" id)
                       "")
                   "-provision.log")))

(define (gascity-supervisor-provision-shepherd-service config)
  "Return the one-shot Shepherd service of the supervisor instance CONFIG:
it runs the provision program as the service user and waits for it.

`spawn-command' with `#:user'/`#:group' waits for the process to exit and
runs it as the service user, unlike `make-forkexec-constructor', which would
return before genesis finished and let the supervisor start too early
(following `postgresql-role-shepherd-service')."
  (let ((user (gascity-supervisor-user config))
        (group (gascity-supervisor-group config)))
    (shepherd-service
     (documentation
      "Provision a Gas City city: genesis, site binding, packs and registry.")
     (provision (list (gascity-supervisor-provision config)))
     (requirement '(user-processes networking))
     (one-shot? #t)
     (start #~(lambda args
                (zero? (spawn-command
                        (list #$(gascity-supervisor-provision-program config))
                        #:user #$user
                        #:group #$group))))
     (stop #~(const #f)))))

(define (gascity-supervisor-shepherd-service config)
  "Return the long-running Shepherd service of the supervisor instance
CONFIG: the secrets-loading wrapper that execs `gc supervisor run'."
  (let ((user (gascity-supervisor-user config))
        (group (gascity-supervisor-group config))
        (home (gascity-supervisor-state-directory config)))
    (shepherd-service
     (documentation
      "Run a Gas City machine-wide supervisor (gc supervisor run).")
     (provision (list (gascity-supervisor-shepherd-service-name config)))
     (requirement `(user-processes networking
                    ,(gascity-supervisor-provision config)))
     (start #~(make-forkexec-constructor
               (list #$(gascity-supervisor-program config))
               #:user #$user
               #:group #$group
               #:directory #$home
               #:log-file #$(gascity-supervisor-log-file config)
               #:environment-variables #$(gascity-supervisor-environment config)))
     (stop #~(make-kill-destructor))
     (respawn? #t))))

(define (gascity-supervisor-shepherd-services value)
  "Return the Shepherd services of the supervisor instances of VALUE: for
each instance, the provision one-shot followed by the supervisor, which
requires it."
  (append-map (lambda (config)
                (list (gascity-supervisor-provision-shepherd-service config)
                      (gascity-supervisor-shepherd-service config)))
              (gascity-supervisor-configurations value)))


;;;
;;; Accounts, activation and profile.
;;;

(define (gascity-supervisor-accounts value)
  "Return the accounts and groups of the supervisor instances of VALUE: one
system account per distinct user, with the state directory of its first
instance as home, and one system group per distinct group.  Instances may
share a user: the account's primary group is that of its first instance and
its supplementary groups are the union of the groups of all its instances
(so a user serving an instance with a distinct `group' still reaches that
group).  Root is left to the operator."
  (define configs
    (gascity-supervisor-configurations value))

  (define users
    (delete-duplicates (map gascity-supervisor-user configs)))

  (define groups
    (delete-duplicates (map gascity-supervisor-group configs)))

  (append
   (map (lambda (name)
          (user-group (name name) (system? #t)))
        (delete "root" groups))
   (filter-map
    (lambda (user)
      (and (not (string=? user "root"))
           (let* ((instances
                   (filter (lambda (config)
                             (string=? user (gascity-supervisor-user config)))
                           configs))
                  (first (car instances))
                  (group (gascity-supervisor-group first)))
             (user-account
              (name user)
              (group group)
              (supplementary-groups
               (delete group
                       (delete-duplicates
                        (map gascity-supervisor-group instances))))
              (system? #t)
              (comment "Gas City supervisor")
              (home-directory (gascity-supervisor-state-directory first))
              (shell (file-append shadow "/sbin/nologin"))))))
    users)))

(define (gascity-supervisor-activation value)
  "Return the activation gexp of VALUE: run each instance's activation
program as root (the activation service runs as root)."
  (define configs
    (gascity-supervisor-configurations value))
  #~(begin
      (use-modules (guix build utils))
      #$@(map (lambda (config)
                #~(invoke #$(gascity-supervisor-activation-program config)))
              configs)))

(define (gascity-supervisor-profile value)
  "Return the packages of the supervisor instances of VALUE."
  (delete-duplicates
   (map gascity-supervisor-configuration-package
        (gascity-supervisor-configurations value))))


;;;
;;; Service type.
;;;

(define gascity-service-type
  (service-type
   (name 'gascity)
   (extensions
    (list (service-extension account-service-type
                             gascity-supervisor-accounts)
          (service-extension activation-service-type
                             gascity-supervisor-activation)
          (service-extension shepherd-root-service-type
                             gascity-supervisor-shepherd-services)
          (service-extension profile-service-type
                             gascity-supervisor-profile)))
   (compose concatenate)
   (extend append)
   (default-value (list (gascity-supervisor-configuration)))
   (description
    "Run machine-wide Gas City supervisors (gc supervisor run) as daemons.
The value is a list of @code{gascity-supervisor-configuration} records, one
per supervisor process.  Each instance has its own @code{gc-home} (the
registry, settings, control socket and instance lock live under it) and its
own API @code{port}, so several supervisors coexist on one machine: a lone
instance defaults to port 8372, and every instance of a multi-instance
service must set @code{port} explicitly.  A paired one-shot Shepherd service
provisions each instance's cities (genesis, site binding, optional pack
fetching and the @file{cities.toml} registry) as the service user before the
supervisor starts.  Use @code{gascity-service-config} to configure the common
single-instance case, and pass @code{'()} when only extending a configured
service with further instances.")))

