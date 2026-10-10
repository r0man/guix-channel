;;; A Gas City supervisor that commits and opens pull requests as a GitHub
;;; App, never as the operator.
;;;
;;; The "gascity" account runs the supervisor and its agents.  It never holds
;;; a long-lived GitHub credential:
;;;
;;;   - `github-app-token-service-type' runs as its own `github-app-token'
;;;     account.  Only that account reads the App's private key.  It mints a
;;;     1-hour installation token narrowed to the listed repositories and
;;;     permissions, and writes it to /run/github-app-token/gascity/token,
;;;     readable by the "gascity" group.  A timer re-mints it every 15
;;;     minutes, once it is less than 30 minutes from expiry;
;;;   - `github-git-config' is the read-only git configuration of the agents
;;;     (exported as GIT_CONFIG_GLOBAL).  It sets the App's bot identity,
;;;     answers git's credential requests for https://github.com from the
;;;     token file, and rewrites SSH GitHub URLs to HTTPS, so no SSH key is
;;;     ever needed;
;;;   - `github-token-gh' wraps gh so that it reads GH_TOKEN from the same
;;;     file.  It comes before any other gh in the supervisor's `packages',
;;;     because PATH is first-wins;
;;;   - `guix-home-service-type' gives the account the same git
;;;     configuration in `~/.config/git/config', for an operator debugging
;;;     as the user (`sudo -iu gascity').
;;;
;;; Replace the App's client ID, the repositories, the bot identity and the
;;; private key path with your own; see "GitHub identity" in
;;; docs/gascity-service.org for registering the App and the rulesets that
;;; must protect `main'.  In a real system the private key comes from
;;; sops-guix, for example:
;;;
;;;   (service sops-secrets-service-type
;;;            (sops-service-configuration
;;;             (secrets
;;;              (list (sops-secret
;;;                     (key '("gascity-app.pem"))
;;;                     (file %secrets.yaml)
;;;                     (user "github-app-token")
;;;                     (permissions #o400))))))
;;;
;;; with the minter's `shepherd-requirement' naming that secret's Shepherd
;;; service.
;;;
;;; Build it like the other examples:
;;;
;;;   guix system build -L modules examples/gascity/github-app.scm

(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu packages version-control)
             (gnu services)
             (gnu services base)
             (gnu services guix)
             (gnu system)
             (gnu tests)
             (r0man guix services gascity)
             (r0man guix services github-app-token)
             (r0man guix services github-token))

;;; The App's bot identity: `<slug>[bot]' and
;;; `<bot-user-id>+<slug>[bot]@users.noreply.github.com', where the bot user
;;; id is the `id' of https://api.github.com/users/<slug>%5Bbot%5D.
(define %bot-name "my-gascity-app[bot]")
(define %bot-email "123456789+my-gascity-app[bot]@users.noreply.github.com")

(define %token-minter
  (github-app-token-configuration
   (name "gascity")                     ;the 'github-app-token-gascity service
   (client-id "Iv23liEXAMPLE")
   ;; The installation is looked up from a repository it covers; set
   ;; `installation-id' instead to skip that request.
   (repository "r0man/guix-channel")
   (private-key-file "/run/secrets/gascity-app.pem")
   ;; The city's own group, and nobody else's.
   (reader-group "gascity")
   ;; Mandatory narrowing: the token can only reach these repositories,
   ;; with these permissions.  `workflows', `administration', `secrets',
   ;; `environments', `pages' and `actions: write' are refused.
   (repositories '("guix-channel"))
   (permissions '((contents . "write")
                  (pull_requests . "write")
                  (checks . "read")
                  (statuses . "read")
                  (actions . "read")))))

(define %token-file
  (github-app-token-file %token-minter))

(define %git-config
  (github-git-config #:name %bot-name
                     #:email %bot-email
                     #:token-file %token-file))

(operating-system
 (inherit
  (simple-operating-system
   (service static-networking-service-type
            (list %qemu-static-networking))
   (service github-app-token-service-type (list %token-minter))
   (service gascity-service-type
            (gascity-service-config
             (dolt-user-name %bot-name)
             (dolt-user-email %bot-email)
             ;; Start after the first token was minted (the one-shot always
             ;; succeeds; the timer heals a failed boot-time mint).
             (shepherd-requirement
              (list (github-app-token-shepherd-service-name %token-minter)))
             (git-config %git-config)
             (packages (list (github-token-gh %token-file github-cli)))
             (cities
              (list
               (gascity-city-configuration
                (name "guix-channel")
                (install-packs? #f)
                (workspace (gascity-workspace-configuration
                            (name "guix-channel")
                            (provider "claude")))
                (providers
                 (list (cons "claude"
                             (gascity-provider-configuration
                              (base "builtin:claude"))))))))))
   ;; The same git configuration for interactive sessions as the account.
   ;; An explicit, minimal service list: `%base-home-services' would start a
   ;; user Shepherd from login shells.
   (service guix-home-service-type
            `(("gascity"
               ,(home-environment
                 (services
                  (list (service home-xdg-configuration-files-service-type
                                 `(("git/config" ,%git-config))))))))))))
