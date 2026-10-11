;;; A Home Gas City configuration for `scripts/gascity-container-smoke-test
;;; parity', which runs it with the real `gc' in `guix home container' and
;;; checks what the burningswell migration relies on:
;;;
;;;   - a read-only `git-config' (GIT_CONFIG_GLOBAL is a store file): genesis
;;;     still succeeds, because the generated global configuration declares
;;;     the `beads.role' gc's bd lifecycle script would otherwise write;
;;;   - a directory source installed into the city root (`"."') and a
;;;     `gascity-symlink' rig file;
;;;   - a declared rig: the controller initializes its beads store once the
;;;     supervisor starts the city, without any `gc rig add'.
;;;
;;; Paths are resolved at build time from XDG_STATE_HOME, which the smoke test
;;; exports for both the build and the container.

(use-modules (gnu home)
             (gnu home services)
             (gnu services)
             (guix gexp)
             (r0man guix home services gascity))

(define %state
  (or (getenv "XDG_STATE_HOME")
      (error "gascity-parity-home: XDG_STATE_HOME must be set")))

(define %pin
  ;; The pin the gc binary pre-seeds, so nothing is fetched.
  "sha:f895c0ff47d6ee9334ed282a416387eb5b084d24")

(define %city-pack
  ;; A city pack tree: a formula and an agent prompt.
  (computed-file
   "parity-city-pack"
   #~(begin
       (mkdir #$output)
       (mkdir (string-append #$output "/formulas"))
       (call-with-output-file
           (string-append #$output "/formulas/parity-check.toml")
         (lambda (port)
           (display "formula = \"parity-check\"\nversion = 1\n\n[[steps]]\nid = \"noop\"\ntitle = \"Nothing to do\"\n"
                    port)))
       (mkdir (string-append #$output "/prompts"))
       (call-with-output-file
           (string-append #$output "/prompts/parity.md")
         (lambda (port)
           (display "Parity prompt.\n" port))))))

(home-environment
 (services
  (append
   (list
    (service home-gascity-service-type
             (gascity-service-config
              (dolt-user-name "Gas City")
              (dolt-user-email "gascity@example.com")
              (git-config (plain-file "parity-git-config"
                                      "[user]\n\tname = Parity Agent\n\temail = parity@example.com\n"))
              (cities
               (list
                (gascity-city-configuration
                 (name "parity")
                 (install-packs? #f)
                 (workspace (gascity-workspace-configuration
                             (name "parity")
                             (provider "claude")))
                 (providers
                  (list (cons "claude"
                              (gascity-provider-configuration
                               (base "builtin:claude")))))
                 (rigs (list (gascity-rig-configuration
                              (name "app")
                              (path (string-append %state "/rig"))
                              (prefix "ap"))))
                 (pack (gascity-pack-configuration
                        (name "parity")
                        (schema 2)
                        (imports
                         (list
                          (cons "core"
                                (gascity-import-configuration
                                 (source "https://github.com/gastownhall/gascity.git//internal/bootstrap/packs/core")
                                 (version %pin)))
                          (cons "bd"
                                (gascity-import-configuration
                                 (source "https://github.com/gastownhall/gascity.git//examples/bd")
                                 (version %pin)))))))
                 (files (list (cons "." %city-pack)))
                 (rig-files
                  (list (cons "README.md" (plain-file "rig-readme" "A rig.\n"))
                        (cons ".gc/scripts/parity.md"
                              (gascity-symlink
                               (string-append %state
                                              "/gascity/parity/prompts/parity.md")))))))))))
   %base-home-services)))
