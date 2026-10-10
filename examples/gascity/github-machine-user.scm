;;; A Gas City supervisor that commits and opens pull requests as a GitHub
;;; machine user, never as the operator.
;;;
;;; Use this when the agents contribute to repositories you do not own
;;; (forks, upstream pull requests), which a GitHub App installed on your own
;;; account cannot do; for your own repositories prefer the App
;;; (examples/gascity/github-app.scm), whose tokens live for an hour.
;;;
;;; The machine user is a second GitHub account (GitHub's terms allow one free
;;; machine account per person), invited as a collaborator.  Fine-grained
;;; personal access tokens do not work for a collaborator on a personal
;;; account's repositories, so it authenticates with a *classic* token:
;;; scope `public_repo' (or `repo' for private repositories) and nothing
;;; else -- in particular not `workflow' -- expiring within 90 days.
;;;
;;; The token is a file, readable only by the "gascity" account.  The same
;;; consumer helpers as for the App read it:
;;;
;;;   - `github-git-config' is the read-only git configuration of the agents
;;;     (exported as GIT_CONFIG_GLOBAL): the machine user's identity, a
;;;     credential helper that answers only for https://github.com, and
;;;     SSH-to-HTTPS URL rewrites, so no SSH key is ever needed;
;;;   - `github-token-gh' wraps gh so that it reads GH_TOKEN from the file.
;;;
;;; In a real system the token comes from sops-guix, for example:
;;;
;;;   (service sops-secrets-service-type
;;;            (sops-service-configuration
;;;             (secrets
;;;              (list (sops-secret
;;;                     (key '("gascity-github-token"))
;;;                     (file %secrets.yaml)
;;;                     (user "gascity")
;;;                     (permissions #o400))))))
;;;
;;; with the supervisor's `shepherd-requirement' naming that secret's
;;; Shepherd service.  See "GitHub identity" in docs/gascity-service.org.
;;;
;;;   guix system build -L modules examples/gascity/github-machine-user.scm

(use-modules (gnu)
             (gnu packages version-control)
             (gnu services)
             (gnu services base)
             (gnu system)
             (gnu tests)
             (r0man guix services gascity)
             (r0man guix services github-token))

;;; The machine user's identity.  Keep its email private on GitHub and
;;; enable "Block command line pushes that expose my email"; commits then
;;; use the `<id>+<login>@users.noreply.github.com' address.
(define %login "my-gascity-bot")
(define %email "123456789+my-gascity-bot@users.noreply.github.com")

;;; The classic token, decrypted by sops-guix in a real system.
(define %token-file "/run/secrets/gascity-github-token")

(operating-system
 (inherit
  (simple-operating-system
   (service static-networking-service-type
            (list %qemu-static-networking))
   (service gascity-service-type
            (gascity-service-config
             (dolt-user-name %login)
             (dolt-user-email %email)
             (git-config (github-git-config #:name %login
                                            #:email %email
                                            #:token-file %token-file))
             (packages (list (github-token-gh %token-file github-cli)))
             (cities
              (list
               (gascity-city-configuration
                (name "upstream")
                (install-packs? #f)
                (workspace (gascity-workspace-configuration
                            (name "upstream")
                            (provider "claude")))
                (providers
                 (list (cons "claude"
                             (gascity-provider-configuration
                              (base "builtin:claude")))))))))))))
