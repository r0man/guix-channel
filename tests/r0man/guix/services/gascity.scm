;;; Unit tests for the Gas City records and TOML serialization in
;;; (r0man guix services gascity): the golden `city.toml' and `pack.toml' of
;;; the design's worked examples, the swarm and gastown shapes through the
;;; typed core plus `extra-config', the shared serializer helpers, the
;;; `extra-config' precedence merge (replacement, recursive table merge and
;;; duplicate rejection), `extra-toml' verbatim append, symbol keys, `files'
;;; and `rig-files', and the literal-secret guard.
;;;
;;; `gascity-city->toml-string' and friends return a gexp, so the golden tests
;;; reduce it to a plain string with `eval-gexp' -- the same trick the TOML
;;; tests use -- before comparing.

(define-module (test-r0man-guix-services-gascity)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (guix gexp)
  #:use-module (r0man guix services gascity)
  #:use-module (r0man guix toml)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim))

(define (eval-gexp x)
  "Get the serialized config as a string."
  (eval (gexp->approximate-sexp x)
        (current-module)))

(define (render document)
  "Return the TOML text that DOCUMENT lowers to."
  (eval-gexp (toml-document->string document)))

(define (render-nodes nodes)
  "Return the TOML text that NODES, a list of TOML nodes, lowers to."
  (render (apply toml-document nodes)))

(test-begin "gascity")


;;;
;;; Shared serializer helpers.
;;;

(test-group "serializer helpers"
  (test-equal "a string, an integer, a real and a boolean"
    (string-append "s = \"x\"\n"
                   "i = 3\n"
                   "f = 1.5\n"
                   "b = true\n")
    (render-nodes
     (append (gascity-serialize-string "s" "x")
             (gascity-serialize-integer "i" 3)
             (gascity-serialize-real "f" 1.5)
             (gascity-serialize-boolean "b" #t))))

  (test-equal "maybe-* helpers skip 'unset"
    "a = \"kept\"\n"
    (render-nodes
     (append (gascity-serialize-maybe-string "gone" 'unset)
             (gascity-serialize-maybe-integer "gone-too" 'unset)
             (gascity-serialize-maybe-boolean "gone-three" 'unset)
             (gascity-serialize-maybe-real "gone-four" 'unset)
             (gascity-serialize-maybe-string "a" "kept"))))

  (test-equal "the tri-state helper skips 'unset and emits #t/#f"
    (string-append "on = true\n"
                   "off = false\n")
    (render-nodes
     (append (gascity-serialize-tri-state "absent" 'unset)
             (gascity-serialize-tri-state "on" #t)
             (gascity-serialize-tri-state "off" #f))))

  (test-equal "a list of strings is a TOML array and a string map an inline table"
    (string-append "xs = [\"a\", \"b\"]\n"
                   "env = { FOO = \"bar\", BAZ = \"qux\" }\n")
    (render-nodes
     (append (gascity-serialize-list-of-strings "xs" '("a" "b"))
             (gascity-serialize-string-map
              "env" '(("FOO" . "bar") ("BAZ" . "qux"))))))

  (test-equal "a string-or-gexp splices a gexp between the quotes"
    "cmd = \"spliced\"\n"
    (render-nodes
     (gascity-serialize-string-or-gexp "cmd" #~(string-append "spli" "ced"))))

  (test-equal "empty-serializer returns nothing"
    '()
    (gascity-empty-serializer "any" "args")))


;;;
;;; Golden documents for the design's worked examples.
;;;

(test-group "golden swarm city.toml"
  (test-equal "the §11 swarm mapping"
    (string-append "[workspace]\n"
                   "name = \"swarm\"\n"
                   "provider = \"claude\"\n"
                   "\n"
                   "[providers]\n"
                   "\n"
                   "[providers.claude]\n"
                   "base = \"builtin:claude\"\n"
                   "\n"
                   "[daemon]\n"
                   "patrol_interval = \"30s\"\n"
                   "max_restarts = 5\n"
                   "restart_window = \"1h\"\n"
                   "shutdown_timeout = \"5s\"\n"
                   "\n"
                   "[[rigs]]\n"
                   "name = \"checkout-service\"\n"
                   "prefix = \"cs\"\n"
                   "formula_vars = { push = \"true\", open_pr = \"true\", "
                   "drain_policy = \"separate\" }\n")
    (eval-gexp
     (gascity-city->toml-string
      (gascity-city-configuration
       (name "swarm")
       (directory "/var/lib/gascity/swarm")
       (workspace (gascity-workspace-configuration
                   (name "swarm")
                   (provider "claude")))
       (providers
        (list (cons "claude"
                    (gascity-provider-configuration (base "builtin:claude")))))
       (daemon (gascity-daemon-configuration
                (patrol-interval "30s")
                (max-restarts 5)
                (restart-window "1h")
                (shutdown-timeout "5s")))
       (rigs (list (gascity-rig-configuration
                    (name "checkout-service")
                    (prefix "cs")
                    (formula-vars '(("push" . "true")
                                    ("open_pr" . "true")
                                    ("drain_policy" . "separate")))))))))))

(test-group "golden swarm pack.toml"
  (test-equal "the §11 swarm pack"
    (string-append "[pack]\n"
                   "name = \"swarm\"\n"
                   "schema = 2\n"
                   "\n"
                   "[imports]\n"
                   "\n"
                   "[imports.core]\n"
                   "source = \"https://github.com/gastownhall/gascity.git"
                   "//internal/bootstrap/packs/core\"\n"
                   "version = \"sha:f895c0ff47d6ee9334ed282a416387eb5b084d24\"\n"
                   "\n"
                   "[imports.bd]\n"
                   "source = \"https://github.com/gastownhall/gascity.git"
                   "//examples/bd\"\n"
                   "version = \"sha:f895c0ff47d6ee9334ed282a416387eb5b084d24\"\n"
                   "\n"
                   "[imports.swarm]\n"
                   "source = \"packs/swarm\"\n")
    (eval-gexp
     (gascity-pack->toml-string
      (gascity-pack-configuration
       (name "swarm")
       (schema 2)
       (imports
        (list
         (cons "core"
               (gascity-import-configuration
                (source "https://github.com/gastownhall/gascity.git//internal/bootstrap/packs/core")
                (version "sha:f895c0ff47d6ee9334ed282a416387eb5b084d24")))
         (cons "bd"
               (gascity-import-configuration
                (source "https://github.com/gastownhall/gascity.git//examples/bd")
                (version "sha:f895c0ff47d6ee9334ed282a416387eb5b084d24")))
         (cons "swarm"
               (gascity-import-configuration (source "packs/swarm"))))))))))

(test-group "the §11 agent example, on the typed core"
  (test-equal "an agent in pack.toml, in schema declaration order"
    (string-append "[[agent]]\n"
                   "name = \"mayor\"\n"
                   "scope = \"city\"\n"
                   "prompt_template = \"agents/mayor/prompt.template.md\"\n"
                   "provider = \"claude\"\n"
                   "env = { GC_TARGET_BRANCH = \"$GC_TARGET_BRANCH\" }\n"
                   "option_defaults = { model = \"claude-sonnet-5-5\", "
                   "effort = \"medium\" }\n"
                   "idle_timeout = \"30m\"\n"
                   "inject_fragments = [\"bs-guix\", \"bs-gates\"]\n")
    (eval-gexp
     (gascity-pack->toml-string
      (gascity-pack-configuration
       (agents
        (list (gascity-agent-configuration
               (name "mayor")
               (scope "city")
               (provider "claude")
               (prompt-template "agents/mayor/prompt.template.md")
               (inject-fragments '("bs-guix" "bs-gates"))
               (option-defaults '(("model" . "claude-sonnet-5-5")
                                  ("effort" . "medium")))
               (idle-timeout "30m")
               (env '(("GC_TARGET_BRANCH" . "$GC_TARGET_BRANCH")))))))))))

(test-group "golden gastown city.toml"
  (test-equal "typed core plus extra-config for the mail table"
    (string-append "[workspace]\n"
                   "name = \"gastown\"\n"
                   "provider = \"claude\"\n"
                   "global_fragments = [\"command-glossary\", "
                   "\"operational-awareness\"]\n"
                   "\n"
                   "[providers]\n"
                   "\n"
                   "[providers.claude]\n"
                   "base = \"builtin:claude\"\n"
                   "\n"
                   "[patches]\n"
                   "\n"
                   "[[patches.agent]]\n"
                   "name = \"mayor\"\n"
                   "max_session_age = \"6h\"\n"
                   "max_session_age_jitter = \"15m\"\n"
                   "\n"
                   "[[patches.agent]]\n"
                   "name = \"deacon\"\n"
                   "max_session_age = \"6h\"\n"
                   "max_session_age_jitter = \"15m\"\n"
                   "\n"
                   "[daemon]\n"
                   "formula_v2 = true\n"
                   "patrol_interval = \"30s\"\n"
                   "max_restarts = 5\n"
                   "restart_window = \"1h\"\n"
                   "shutdown_timeout = \"5s\"\n"
                   "\n"
                   "[defaults]\n"
                   "\n"
                   "[defaults.rig]\n"
                   "\n"
                   "[defaults.rig.imports]\n"
                   "\n"
                   "[defaults.rig.imports.gastown]\n"
                   "source = \"https://github.com/gastownhall/gascity-packs"
                   "/tree/main/gastown\"\n"
                   "version = \"sha:33d3a430a67d1782ad364556cb566bdb01d0afe3\"\n"
                   "\n"
                   "[mail]\n"
                   "provider = \"smtp\"\n")
    (eval-gexp
     (gascity-city->toml-string
      (gascity-city-configuration
       (name "gastown")
       (workspace (gascity-workspace-configuration
                   (name "gastown")
                   (provider "claude")
                   (global-fragments '("command-glossary"
                                       "operational-awareness"))))
       (providers
        (list (cons "claude"
                    (gascity-provider-configuration (base "builtin:claude")))))
       (defaults
        (gascity-pack-defaults-configuration
         (rig (gascity-pack-rig-defaults-configuration
               (imports
                (list (cons "gastown"
                            (gascity-import-configuration
                             (source "https://github.com/gastownhall/gascity-packs/tree/main/gastown")
                             (version "sha:33d3a430a67d1782ad364556cb566bdb01d0afe3")))))))))
       (daemon (gascity-daemon-configuration
                (patrol-interval "30s")
                (max-restarts 5)
                (restart-window "1h")
                (shutdown-timeout "5s")
                (formula-v2 #t)))
       (patches (gascity-patches-configuration
                 (agents (list (gascity-agent-patch-configuration
                                (name "mayor")
                                (max-session-age "6h")
                                (max-session-age-jitter "15m"))
                               (gascity-agent-patch-configuration
                                (name "deacon")
                                (max-session-age "6h")
                                (max-session-age-jitter "15m"))))))
       (extra-config (list (toml-table 'mail (toml-field 'provider "smtp")))))))))


;;;
;;; Escape hatches.
;;;

(test-group "extra-config precedence"
  (test-equal "a field at the same key replaces the generated field and a \
table merges recursively"
    (string-append "[workspace]\n"
                   "name = \"b\"\n"
                   "provider = \"claude\"\n")
    (eval-gexp
     (gascity-city->toml-string
      (gascity-city-configuration
       (name "m")
       (workspace (gascity-workspace-configuration
                   (name "a")
                   (provider "claude")))
       (extra-config
        (list (toml-table "workspace" (toml-field "name" "b"))))))))

  (test-equal "symbol keys work everywhere a key is accepted"
    (string-append "[experimental]\n"
                   "feature_flag = \"true\"\n")
    (eval-gexp
     (gascity-city->toml-string
      (gascity-city-configuration
       (name "s")
       (extra-config
        (list (toml-table 'experimental (toml-field 'feature_flag "true"))))))))

  (test-assert "a key declared twice at one level in extra-config is an error"
    (guard (condition (#t #t))
      (gascity-merge-extra-config
       (list (toml-field "a" 1))
       (list (toml-field "a" 2) (toml-field "a" 3)))
      #f))

  (test-equal "gascity-merge-extra-config leaves the generated nodes untouched \
without extra-config"
    (list (toml-field "a" 1))
    (gascity-merge-extra-config (list (toml-field "a" 1)) '())))

(test-group "extra-toml"
  (test-equal "a raw string is appended verbatim after the generated document"
    (string-append "[workspace]\n"
                   "name = \"t\"\n"
                   "[extra_table]\n"
                   "k = 1\n")
    (eval-gexp
     (gascity-city->toml-string
      (gascity-city-configuration
       (name "t")
       (workspace (gascity-workspace-configuration (name "t")))
       (extra-toml "[extra_table]\nk = 1\n")))))

  (test-equal "no extra-toml appends nothing"
    "[workspace]\nname = \"t\"\n"
    (eval-gexp
     (gascity-city->toml-string
      (gascity-city-configuration
       (name "t")
       (workspace (gascity-workspace-configuration (name "t")))
       (extra-toml #f))))))

(test-group "files and rig-files"
  (test-assert "files and rig-files are accepted data and are never emitted"
    (let* ((city (gascity-city-configuration
                  (name "f")
                  (files (list (cons "prompts/a.md" (plain-file "a" "A"))))
                  (rig-files (list (cons "b.sh" (plain-file "b" "B"))))))
           (rendered (eval-gexp (gascity-city->toml-string city))))
      (and (= (length (gascity-city-configuration-files city)) 1)
           (= (length (gascity-city-configuration-rig-files city)) 1)
           (not (string-contains rendered "prompts/a.md"))
           (not (string-contains rendered "b.sh"))))))


;;;
;;; The literal-secret guard.
;;;

(test-group "secrets"
  (test-assert "a literal Anthropic token is rejected"
    (guard (condition (#t #t))
      (gascity-sanitize-and-validate
       '("ANTHROPIC_API_KEY=sk-ant-abc123def456"))
      #f))

  (test-assert "a literal GitHub token is rejected"
    (guard (condition (#t #t))
      (gascity-sanitize-and-validate
       (list (toml-field "auth_token" "ghp_0123456789abcdef")))
      #f))

  (test-assert "a long high-entropy token is rejected"
    (guard (condition (#t #t))
      (gascity-sanitize-and-validate
       (list (toml-field "token" "AbCdEf0123456789AbCdEf0123456789AbCdEf0123456789")))
      #f))

  (test-assert "a $VAR reference is accepted"
    (gascity-sanitize-and-validate '("ANTHROPIC_API_KEY=$ANTHROPIC_API_KEY")))

  (test-assert "a sha: pin and a builtin: name are accepted"
    (gascity-sanitize-and-validate
     (list (toml-field "version" "sha:f895c0ff47d6ee9334ed282a416387eb5b084d24")
           (toml-field "base" "builtin:claude"))))

  (test-assert "a literal secret in a city document is rejected at emission"
    (guard (condition (#t #t))
      (gascity-city->toml-document
       (gascity-city-configuration
        (name "secret")
        (workspace (gascity-workspace-configuration
                    (env '(("ANTHROPIC_API_KEY" . "sk-ant-abc123")))))))
      #f)))


;;;
;;; Optional Gas City validation.
;;;

;; `gc' is a Go binary from the `gascity-next' package; it is not part of the
;; test environment, so this test skips cleanly when it cannot be found.  When
;; it is found, the generated swarm pack and a trivial city are written to a
;; temporary directory and validated with `gc config show --validate', which
;; resolves the city from the current directory and needs no network for a
;; city that imports nothing.

(define (find-gc)
  "Return the `gc' program name or #f: $GASCITY_GC, or `gc' from PATH."
  (or (getenv "GASCITY_GC")
      (let ((port (false-if-exception
                   (open-input-pipe "command -v gc 2>/dev/null"))))
        (and port
             (let ((line (read-line port)))
               (close-pipe port)
               (and line
                    (not (eof-object? line))
                    (not (string-null? line))
                    line))))))

(test-group "gc config show --validate"
  (let ((gc (find-gc)))
    (if (not gc)
        (begin
          (test-skip 1)
          (test-assert "gc is available" #f))
        (let* ((directory (string-append "/tmp/gascity-validate-"
                                         (number->string (getpid))))
               (old-directory (getcwd))
               (city (gascity-city-configuration
                      (name "swarm")
                      (workspace (gascity-workspace-configuration
                                  (name "swarm")
                                  (provider "claude")))
                      (providers
                       (list (cons "claude"
                                   (gascity-provider-configuration
                                    (base "builtin:claude")))))
                      (daemon (gascity-daemon-configuration
                               (patrol-interval "30s")
                               (max-restarts 5)))))
               (cleanup (lambda ()
                          (false-if-exception (delete-file
                                               (string-append directory
                                                              "/city.toml")))
                          (false-if-exception (delete-file
                                               (string-append directory
                                                              "/pack.toml")))
                          (false-if-exception (rmdir directory)))))
          (dynamic-wind
            (lambda () (mkdir directory))
            (lambda ()
              (call-with-output-file (string-append directory "/city.toml")
                (lambda (port)
                  (display (eval-gexp (gascity-city->toml-string city)) port)))
              (chdir directory)
              (let ((status (false-if-exception
                             (system (string-append gc
                                                    " config show --validate")))))
                (chdir old-directory)
                (test-assert "the generated city validates with gc"
                  (and (integer? status) (zero? status)))))
            cleanup)))))

;;;
;;; Supervisor configurations and validation (Phase 3, §13.1-§13.2, §16.15).
;;;

(test-group "supervisor configurations"
  (test-equal "a lone instance defaults to port 8372"
    '(8372)
    (map gascity-supervisor-configuration-port
         (gascity-supervisor-configurations
          (list (gascity-supervisor-configuration)))))

  (test-equal "gascity-service-config wraps a single record in a list"
    '(8372)
    (map gascity-supervisor-configuration-port
         (gascity-supervisor-configurations
          (gascity-service-config (id "solo")))))

  (test-equal "two instances keep their explicit ports"
    '(8372 8373)
    (map gascity-supervisor-configuration-port
         (gascity-supervisor-configurations
          (list (gascity-supervisor-configuration (id "a") (gc-home "/gc-a") (port 8372))
                (gascity-supervisor-configuration (id "b") (gc-home "/gc-b") (port 8373))))))

  (test-assert "two instances require an explicit port"
    (guard (condition (#t #t))
      (gascity-supervisor-configurations
       (list (gascity-supervisor-configuration (id "a") (gc-home "/gc-a"))
             (gascity-supervisor-configuration (id "b") (gc-home "/gc-b") (port 8373))))
      #f))

  (test-assert "an invalid id is rejected"
    (guard (condition (#t #t))
      (gascity-supervisor-configurations
       (list (gascity-supervisor-configuration (id "bad/id"))))
      #f))

  (test-assert "a non-list value is rejected"
    (guard (condition (#t #t))
      (gascity-supervisor-configurations "not-a-list")
      #f))

  (test-assert "a non-record element is rejected"
    (guard (condition (#t #t))
      (gascity-supervisor-configurations
       (list (gascity-supervisor-configuration) "nope"))
      #f))

  (test-assert "a duplicate gc-home is rejected"
    (guard (condition (#t #t))
      (gascity-supervisor-configurations
       (list (gascity-supervisor-configuration (id "a") (gc-home "/gc") (port 8372))
             (gascity-supervisor-configuration (id "b") (gc-home "/gc") (port 8373))))
      #f))

  (test-assert "a duplicate (bind, port) is rejected"
    (guard (condition (#t #t))
      (gascity-supervisor-configurations
       (list (gascity-supervisor-configuration (id "a") (gc-home "/gc-a") (port 8372))
             (gascity-supervisor-configuration (id "b") (gc-home "/gc-b") (port 8372))))
      #f)))


;;;
;;; `cities.toml' locked read-merge-write (Phase 3, §16.3, §16.8).
;;;

(test-group "cities.toml merge"
  (define existing
    (string-append "# a comment\n\n"
                   "[[rigs]]\nname = \"r1\"\npath = \"/p\"\n\n"
                   "[[pending_city_requests]]\npath = \"/q\"\n"))

  (test-equal "an empty existing file and no cities is the empty string"
    ""
    (gascity-cities-toml-merge "" '()))

  (test-equal "a declared city is written and [[rigs]]/pending preserved"
    (string-append "# a comment\n\n"
                   "[[rigs]]\nname = \"r1\"\npath = \"/p\"\n\n"
                   "[[pending_city_requests]]\npath = \"/q\"\n\n"
                   "[[cities]]\npath = \"/new\"\nname = \"new\"\n")
    (gascity-cities-toml-merge existing (list (cons "/new" "new"))))

  (test-equal "a city without a name emits only its path"
    "[[cities]]\npath = \"/p\"\n"
    (gascity-cities-toml-merge "" (list (cons "/p" 'unset))))

  (test-equal "a removed city is unregistered and rigs are kept verbatim"
    "[[rigs]]\nname = \"r1\"\npath = \"/p\"\n"
    (gascity-cities-toml-merge
     (string-append "[[cities]]\npath = \"/a\"\nname = \"a\"\n\n"
                    "[[rigs]]\nname = \"r1\"\npath = \"/p\"\n")
     '()))

  (test-equal "the merge is deterministic (idempotent)"
    (gascity-cities-toml-merge existing (list (cons "/new" "new")))
    (gascity-cities-toml-merge
     (gascity-cities-toml-merge existing (list (cons "/new" "new")))
     (list (cons "/new" "new")))))


;;;
;;; `.gc/site.toml' (Phase 3, §8).
;;;

(test-group "city site.toml"
  (test-equal "workspace name/prefix and [[rig]] entries"
    (string-append "workspace_name = \"swarm\"\n"
                   "workspace_prefix = \"sw\"\n"
                   "\n"
                   "[[rig]]\n"
                   "name = \"checkout-service\"\n"
                   "path = \"/var/lib/gascity/checkout-service\"\n")
    (eval-gexp
     (gascity-city-site->toml-string
      (gascity-city-configuration
       (name "swarm")
       (workspace (gascity-workspace-configuration (name "swarm") (prefix "sw")))
       (rigs (list (gascity-rig-configuration
                    (name "checkout-service")
                    (path "/var/lib/gascity/checkout-service")))))))))


;;;
;;; Accounts, activation and profile (Phase 3, §8, §9.1).
;;;

(define gascity-supervisor-accounts
  (@@ (r0man guix services gascity) gascity-supervisor-accounts))

(define gascity-supervisor-shepherd-services
  (@@ (r0man guix services gascity) gascity-supervisor-shepherd-services))

(test-group "supervisor accounts"
  (test-assert "a default instance creates one user and one group"
    (let ((created (gascity-supervisor-accounts
                    (list (gascity-supervisor-configuration)))))
      (and (= 1 (length (filter user-account? created)))
           (= 1 (length (filter user-group? created)))
           (string=? "gascity"
                     (user-account-name (car (filter user-account? created))))
           (string=? "gascity"
                     (user-group-name (car (filter user-group? created)))))))

  (test-equal "the account's home is the state directory"
    "/var/lib/gascity"
    (let ((created (gascity-supervisor-accounts
                    (list (gascity-supervisor-configuration)))))
      (user-account-home-directory (car (filter user-account? created)))))

  (test-equal "instances sharing a user union their groups as supplementary"
    '("extra")
    (let ((created
           (gascity-supervisor-accounts
            (list (gascity-supervisor-configuration (id "a") (gc-home "/gc-a")
                                                    (port 8372))
                  (gascity-supervisor-configuration (id "b") (gc-home "/gc-b")
                                                    (port 8373) (group "extra"))))))
      (user-account-supplementary-groups (car (filter user-account? created))))))

(test-group "supervisor shepherd services"
  (define value
    (list (gascity-supervisor-configuration (id "a") (gc-home "/gc-a") (port 8372))
          (gascity-supervisor-configuration (id "b") (gc-home "/gc-b") (port 8373))))

  (test-equal "a lone instance has unsuffixed names"
    '((gascity-provision) (gascity-supervisor))
    (map shepherd-service-provision
         (gascity-supervisor-shepherd-services
          (list (gascity-supervisor-configuration)))))

  (test-equal "two instances yield four id-suffixed services"
    '((gascity-provision-a) (gascity-supervisor-a)
      (gascity-provision-b) (gascity-supervisor-b))
    (map shepherd-service-provision
         (gascity-supervisor-shepherd-services value)))

  (test-assert "the provision services are one-shot"
    (let ((provisions (filter shepherd-service-one-shot?
                              (gascity-supervisor-shepherd-services value))))
      (and (= 2 (length provisions))
           (lset= equal?
                  '((gascity-provision-a) (gascity-provision-b))
                  (map shepherd-service-provision provisions)))))

  (test-assert "each supervisor requires its paired provision one-shot"
    (let* ((services (gascity-supervisor-shepherd-services value))
           (supervisors (remove shepherd-service-one-shot? services)))
      (and (memq 'gascity-provision-a
                 (shepherd-service-requirement (car supervisors)))
           (memq 'gascity-provision-b
                 (shepherd-service-requirement (cadr supervisors))))))

  (test-assert "the two instances are independent"
    (let* ((value (list (gascity-supervisor-configuration (id "a") (gc-home "/gc-a")
                                                          (port 8372))
                        (gascity-supervisor-configuration (id "b") (gc-home "/gc-b")
                                                          (port 8373))))
           (provisions (filter shepherd-service-one-shot?
                               (gascity-supervisor-shepherd-services value))))
      (and (equal? '(gascity-provision-a)
                   (shepherd-service-provision (car provisions)))
           (equal? '(gascity-provision-b)
                   (shepherd-service-provision (cadr provisions)))))))

(test-end "gascity")
