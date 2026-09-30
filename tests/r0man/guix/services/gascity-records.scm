;;; Unit tests for the generated Gas City leaf records in
;;; (r0man guix services gascity records): the module loads and its records
;;; construct with `unset' defaults, a comprehensive city serializes to a
;;; golden `city.toml', the tri-state, secret and gexp overrides behave, and
;;; -- when a `gc' binary is available -- the generated city validates with
;;; `gc config show --validate'.
;;;
;;; The record serializers return a list of TOML nodes; the golden tests lower
;;; it through `toml-document->string' and reduce that gexp to a plain string
;;; with `eval-gexp' -- the same trick the TOML and gascity tests use.

(define-module (test-r0man-guix-services-gascity-records)
  #:use-module (guix gexp)
  #:use-module (r0man guix services gascity records)
  #:use-module (r0man guix toml)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim))

(define (eval-gexp x)
  "Get the serialized config as a string."
  (eval (gexp->approximate-sexp x) (current-module)))

(define (render document)
  "Return the TOML text that DOCUMENT lowers to."
  (eval-gexp (toml-document->string document)))

(define (render-nodes nodes)
  "Return the TOML text that NODES, a list of TOML nodes, lowers to."
  (render (apply toml-document nodes)))

(test-begin "gascity-records")


;;;
;;; The generated module loads and its records construct.
;;;

(test-group "generated records"
  (test-assert "a city record constructs and its predicate holds"
    (gascity-city-configuration? (gascity-city-configuration)))

  (test-equal "a field defaults to the `unset' marker"
    'unset
    (gascity-daemon-configuration-patrol-interval
     (gascity-daemon-configuration)))

  (test-equal "a set field is readable through its accessor"
    "burningswell"
    (gascity-workspace-configuration-name
     (gascity-workspace-configuration (name "burningswell"))))

  (test-assert "a boolean field holds a tri-state value"
    (eq? #t
         (gascity-agent-configuration-inject-assigned-skills
          (gascity-agent-configuration (inject-assigned-skills #t))))))


;;;
;;; The golden city.
;;;

(test-group "golden city.toml"
  (let ((city (gascity-city-configuration
               (include (list "pack/base"))
               (workspace (gascity-workspace-configuration
                           (name "burningswell")
                           (provider "claude")
                           (start-command #~(string-append
                                             "claude"
                                             " --dangerously-skip-permissions"))
                           (max-active-sessions 8)
                           (suspended #f)
                           (env (list (cons "FOO" "bar")))))
               (providers (list
                           (cons "claude"
                                 (gascity-provider-spec-configuration
                                  (base "builtin:claude")
                                  (command "claude")))))
               (agent (list (gascity-agent-configuration
                             (name "mayor")
                             (provider "claude")
                             (inject-assigned-skills #t))))
               (daemon (gascity-daemon-configuration
                        (formula-v2 #f)
                        (patrol-interval "30s")
                        (max-restarts 5)))
               (upstreams (list
                           (cons "aws"
                                 (gascity-upstream-spec-configuration
                                  (api-key "$AWS_BEDROCK_KEY"))))))))
    (test-equal "a comprehensive city serializes to the golden document"
      (string-append
       "include = [\"pack/base\"]\n"
       "\n"
       "[daemon]\n"
       "formula_v2 = false\n"
       "max_restarts = 5\n"
       "patrol_interval = \"30s\"\n"
       "\n"
       "[providers]\n"
       "\n"
       "[providers.claude]\n"
       "base = \"builtin:claude\"\n"
       "command = \"claude\"\n"
       "\n"
       "[upstreams]\n"
       "\n"
       "[upstreams.aws]\n"
       "api_key = \"$AWS_BEDROCK_KEY\"\n"
       "\n"
       "[workspace]\n"
       "env = { FOO = \"bar\" }\n"
       "max_active_sessions = 8\n"
       "name = \"burningswell\"\n"
       "provider = \"claude\"\n"
       "start_command = \"claude --dangerously-skip-permissions\"\n"
       "suspended = false\n"
       "\n"
       "[[agent]]\n"
       "inject_assigned_skills = true\n"
       "name = \"mayor\"\n"
       "provider = \"claude\"\n")
      (render (apply toml-document
                     (gascity-city-configuration->toml city))))))


;;;
;;; The tri-state override.
;;;

(test-group "tri-state override"
  (test-equal "an unset tri-state field emits nothing"
    ""
    (render-nodes
     (gascity-daemon-configuration->toml (gascity-daemon-configuration))))

  (test-equal "a true tri-state field emits true"
    "formula_v2 = true\n"
    (render-nodes
     (gascity-daemon-configuration->toml
      (gascity-daemon-configuration (formula-v2 #t)))))

  (test-equal "a false tri-state field emits false"
    "formula_v2 = false\n"
    (render-nodes
     (gascity-daemon-configuration->toml
      (gascity-daemon-configuration (formula-v2 #f)))))

  (test-equal "an unset auto_restart_on_drift emits nothing"
    ""
    (render-nodes
     (gascity-daemon-configuration->toml (gascity-daemon-configuration))))

  (test-equal "an explicit auto_restart_on_drift=true emits true"
    "auto_restart_on_drift = true\n"
    (render-nodes
     (gascity-daemon-configuration->toml
      (gascity-daemon-configuration (auto-restart-on-drift #t)))))

  (test-equal "an explicit auto_restart_on_drift=false emits false"
    "auto_restart_on_drift = false\n"
    (render-nodes
     (gascity-daemon-configuration->toml
      (gascity-daemon-configuration (auto-restart-on-drift #f)))))

  (test-equal "an explicit event_hooks=false emits false"
    "event_hooks = false\n"
    (render-nodes
     (gascity-beads-configuration->toml
      (gascity-beads-configuration (event-hooks #f)))))

  (test-equal "an explicit attach=true emits true"
    "attach = true\n"
    (render-nodes
     (gascity-agent-configuration->toml
      (gascity-agent-configuration (attach #t))))))


;;;
;;; The secret override.
;;;

(test-group "secret override"
  (test-equal "an api_key is emitted verbatim as a `$VAR' reference"
    "api_key = \"$AWS_BEDROCK_KEY\"\n"
    (render-nodes
     (gascity-upstream-spec-configuration->toml
      (gascity-upstream-spec-configuration (api-key "$AWS_BEDROCK_KEY")))))

  (test-assert "a literal credential is rejected"
    (guard (condition (#t #t))
      (gascity-upstream-spec-configuration->toml
       (gascity-upstream-spec-configuration
        (api-key "sk-ant-api03-0123456789abcdefghij")))
      #f))

  (test-equal "an env-var-name field is a plain string, not a secret"
    "secret_env = \"MY_WEBHOOK_SECRET\"\n"
    (render-nodes
     (gascity-webhook-verify-configuration->toml
      (gascity-webhook-verify-configuration
       (secret-env "MY_WEBHOOK_SECRET"))))))


;;;
;;; The gexp override.
;;;

(test-group "gexp override"
  (test-equal "a gexp command is spliced between the quotes"
    "start_command = \"spliced\"\n"
    (render-nodes
     (gascity-workspace-configuration->toml
      (gascity-workspace-configuration
       (start-command #~(string-append "spl" "iced"))))))

  (test-equal "a plain string command is emitted as written"
    "start_command = \"claude\"\n"
    (render-nodes
     (gascity-workspace-configuration->toml
      (gascity-workspace-configuration (start-command "claude")))))

  (test-equal "a gexp script path is spliced between the quotes"
    "script = \"/gnu/store/abc-check\"\n"
    (render-nodes
     (gascity-local-doctor-check-configuration->toml
      (gascity-local-doctor-check-configuration
       (script #~(string-append "/gnu/store/" "abc-check"))))))

  (test-equal "a plain string script path is emitted as written"
    "script = \"check.sh\"\n"
    (render-nodes
     (gascity-local-doctor-check-configuration->toml
      (gascity-local-doctor-check-configuration (script "check.sh")))))

  (test-equal "a gexp fix script is spliced between the quotes"
    "fix = \"/gnu/store/abc-fix\"\n"
    (render-nodes
     (gascity-pack-doctor-entry-configuration->toml
      (gascity-pack-doctor-entry-configuration
       (fix #~(string-append "/gnu/store/" "abc-fix"))))))

  (test-equal "a gexp on_boot command is spliced between the quotes"
    "on_boot = \"echo hi\"\n"
    (render-nodes
     (gascity-agent-configuration->toml
      (gascity-agent-configuration
       (on-boot #~(string-append "echo " "hi")))))))


;;;
;;; Gas City validation (skipped when no `gc' binary is available).
;;;

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
        (let* ((directory (string-append "/tmp/gascity-records-validate-"
                                         (number->string (getpid))))
               (old-directory (getcwd))
               (city (gascity-city-configuration
                      (workspace (gascity-workspace-configuration
                                  (name "records")
                                  (provider "claude")))
                      (providers
                       (list (cons "claude"
                                   (gascity-provider-spec-configuration
                                    (base "builtin:claude")))))
                      (daemon (gascity-daemon-configuration
                               (patrol-interval "30s")
                               (max-restarts 5)))))
               (cleanup (lambda ()
                          (false-if-exception
                           (delete-file (string-append directory "/city.toml")))
                          (false-if-exception
                           (delete-file (string-append directory "/pack.toml")))
                          (false-if-exception (rmdir directory)))))
          (dynamic-wind
            (lambda () (mkdir directory))
            (lambda ()
              (call-with-output-file (string-append directory "/city.toml")
                (lambda (port)
                  (display (render (apply toml-document
                                          (gascity-city-configuration->toml
                                           city)))
                           port)))
              (chdir directory)
              (let ((status (false-if-exception
                             (system (string-append gc
                                                    " config show --validate")))))
                (chdir old-directory)
                (test-assert "the generated city validates with gc"
                  (and (integer? status) (zero? status)))))
            cleanup)))))

(test-end "gascity-records")
