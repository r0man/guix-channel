;;; generate-gascity-records.scm --- generate the Gas City leaf record module
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
;;; This script reads the committed Gas City JSON schemas
;;;
;;;   docs/reference/schema/city-schema.json
;;;   docs/reference/schema/pack-schema.json
;;;
;;; and emits a self-contained Guix module of leaf configuration records,
;;; `modules/r0man/guix/services/gascity/records.scm'.  Regenerate it with
;;;
;;;   make generate-gascity-records
;;;
;;; and check that the committed copy is current with
;;;
;;;   make check-gascity-records
;;;
;;; The output is deterministic: the `$defs' and the properties of each type
;;; are sorted, so the generated module is a pure function of the schema
;;; files.  The module is a standalone leaf surface; it deliberately does not
;;; depend on `(r0man guix services gascity)' (see the design doc, §6.3 and
;;; §12 Phase 4).
;;;
;;; Mapping (design doc §6.1):
;;;
;;;   TOML key `snake_case'           -> Scheme field `kebab-case'
;;;   JSON `string'                   -> maybe-string
;;;   JSON `boolean'                  -> tri-state (`unset', #t, #f)
;;;   JSON `integer'                  -> maybe-integer
;;;   JSON `number'                   -> maybe-real
;;;   JSON array of `string'          -> list of strings
;;;   JSON array of `$ref'            -> array-of-tables of records
;;;   JSON object with a string map   -> alist (TOML inline table)
;;;   JSON object with a `$ref' map   -> alist-of-subtables
;;;   JSON `$ref'                     -> subtable
;;;
;;; The schemas cannot express every semantic; `%field-overrides' below is the
;;; hand-maintained table for the exceptions (tri-state booleans, secrets and
;;; gexp/file-like strings).  See its commentary for the entries.
;;;
;;; Code:

(use-modules (ice-9 format)
             (ice-9 match)
             (json)
             (srfi srfi-1)
             (srfi srfi-13))


;;;
;;; Paths.
;;;

(define (script-directory)
  "Return the directory holding this script, resolving symlinks, or #f."
  (let ((path (car (command-line))))
    (and path (false-if-exception (dirname (canonicalize-path path))))))

(define %repository-root
  (let ((directory (script-directory)))
    (if directory (dirname directory) ".")))

(define %schema-directory
  (string-append %repository-root "/docs/reference/schema"))

(define %default-output
  (string-append %repository-root
                 "/modules/r0man/guix/services/gascity/records.scm"))

(define (city-schema-path)
  (string-append %schema-directory "/city-schema.json"))

(define (pack-schema-path)
  (string-append %schema-directory "/pack-schema.json"))


;;;
;;; Reading the schemas.
;;;

(define (read-json path)
  "Read the JSON document at PATH into an s-expression (objects are alists)."
  (call-with-input-file path
    (lambda (port) (json->scm port #:ordered #f))))

(define (ref-name reference)
  "Return the bare type name of REFERENCE, a JSON `$ref' such as
\"#/$defs/Agent\"."
  (car (reverse (string-split reference #\/))))

(define (merge-defs)
  "Return the merged `$defs' of the two schemas as an alist of
(NAME . PROPERTIES-SCHEMA), sorted by NAME.  A type present in both schemas
must be identical; otherwise the merge is ambiguous and we stop."
  (let* ((city (assoc-ref (read-json (city-schema-path)) "$defs"))
         (pack (assoc-ref (read-json (pack-schema-path)) "$defs"))
         (merged (append city '())))
    (for-each
     (lambda (definition)
       (let* ((name (car definition))
              (existing (assoc-ref merged name)))
         (cond
          ((not existing) (set! merged (cons definition merged)))
          ((not (equal? existing (cdr definition)))
           (error "the city and pack schemas define different types" name)))))
     pack)
    (sort merged (lambda (a b) (string<? (car a) (car b))))))


;;;
;;; Name conversion.
;;;

(define (camel->kebab name)
  "Return NAME, a CamelCase type name, as kebab-case.  Insert a `-' before an
upper-case character that either follows a lower-case character or digit, or
follows an upper-case character that is itself followed by a lower-case
character (so runs of capitals are not split)."
  (let* ((characters (string->list name))
         (length (length characters)))
    (list->string
     (let loop ((index 0) (out '()))
       (if (= index length)
           (reverse out)
           (let ((character (list-ref characters index)))
             (if (and (char-upper-case? character)
                      (> index 0)
                      (let ((previous (list-ref characters (- index 1))))
                        (or (char-lower-case? previous)
                            (char-numeric? previous)
                            (and (char-upper-case? previous)
                                 (< (+ index 1) length)
                                 (char-lower-case? (list-ref characters
                                                             (+ index 1)))))))
                 (loop (+ index 1)
                       (cons (char-downcase character) (cons #\- out)))
                 (loop (+ index 1) (cons (char-downcase character) out)))))))))

(define (field->kebab key)
  "Return KEY, a TOML `snake_case' key, as kebab-case.  Every `_' becomes a
`-'."
  (string-map (lambda (character)
                (if (char=? character #\_) #\- character))
              key))

(define (type-kebab name)
  "Return NAME, a schema type name, as the kebab-case stem used in record
names.  A trailing `Config' is dropped (`DaemonConfig' -> `daemon'), so the
record reads `<gascity-daemon-configuration>' rather than the redundant
`<gascity-daemon-config-configuration>'.  A few brand names whose CamelCase
splits badly are pinned in `%type-name-overrides'."
  (or (assoc-ref %type-name-overrides name)
      (camel->kebab
       (if (string-suffix? "Config" name)
           (substring name 0 (- (string-length name) (string-length "Config")))
           name))))

(define %type-name-overrides
  ;; Schema type name -> kebab-case stem, for names the mechanical CamelCase
  ;; split in `camel->kebab' spells at odds with the product name.
  ;;   GitHubConfig        -> github-config
  ;;   GitHubPRMonitor     -> github-pr-monitor
  ;;   GitHubPRMonitorPatch-> github-pr-monitor-patch
  '(("GitHubConfig" . "github")
    ("GitHubPRMonitor" . "github-pr-monitor")
    ("GitHubPRMonitorPatch" . "github-pr-monitor-patch")))

(define (record-name name)
  (string-append "<gascity-" (type-kebab name) "-configuration>"))

(define (constructor-name name)
  (string-append "gascity-" (type-kebab name) "-configuration"))

(define (maker-name name)
  (string-append "make-gascity-" (type-kebab name) "-configuration"))

(define (predicate-name name)
  (string-append "gascity-" (type-kebab name) "-configuration?"))

(define (serializer-name name)
  (string-append "gascity-" (type-kebab name) "-configuration->toml"))

(define (accessor-name type field)
  (string-append "gascity-" (type-kebab type) "-configuration-"
                 (field->kebab field)))


;;;
;;; The override table.
;;;

;; The schema types every field mechanically; these are the semantics it
;; cannot express.  The table maps a category to the TOML keys that carry it,
;; and is keyed by the bare field name so the same semantic applies wherever
;; the key appears.  An override only fires when the field's schema type is
;; compatible (a `tri-state' override needs a boolean, a `secret' or `gexp'
;; override needs a string), so a name collision such as the
;; `ServiceProcessConfig.command' array keeps its array serializer.
;;
;;   tri-state  Boolean fields whose absent/true/false states differ from a
;;              plain false.  The default boolean serializer already emits
;;              tri-state, but listing them keeps the intent explicit and
;;              stable if the default ever changes.
;;
;;   secret     String fields that carry an abstract credential.  They must
;;              never serialize a literal secret; the serializer rejects the
;;              well-known credential shapes and high-entropy tokens and emits
;;              the value verbatim, so `$VAR' references survive.
;;
;;   gexp       String fields that may embed a store path or a `file-like':
;;              commands, scripts and executable paths.  They accept a string
;;              or a gexp.
;;
;; The env-var-NAME fields are deliberately NOT in the `secret' category: a
;; bare variable name ("MY_API_KEY") is not the secret itself, so it is fine
;; as a plain string.  They are `webhook_secret_env', `webhook_secret_key',
;; `secret_env', `secret_key', `bearer_env', `base_url_env', `api_key_env' and
;; `auth_token_env'.  (`UpstreamEnvBinding.api_key' / `.auth_token' name an
;; env-var too; see the note on `api_key' below.)
;;
;; Known deferral: first-class `gascity-formula-configuration',
;; `gascity-order-configuration', skill and mcp records are intentionally not
;; generated.  There is no JSON schema for formula/order files (only the prose
;; specs docs/reference/specs/formula-spec-v{1,2}.md), and the design keeps
;; directory-convention content out of the typed surface for the first
;; iteration (§1 non-goals, §13.9); formulas and orders are supplied through
;; the `files' escape hatch instead.  Do not treat their absence as a schema
;; gap to be filled by hand: revisit only if there is demand (§12 Phase 6).

(define %field-overrides
  '((tri-state
     ;; default-on but omitted when unset, so an auto-generated config never
     ;; pins the default; an explicit false is preserved.
     "formula_v2"
     ;; deprecated alias of formula_v2; an explicit false is preserved.
     "graph_workflows"
     ;; nil defaults to true; false is a global kill switch.
     "auto_restart_on_drift"
     ;; nil defaults to true; false retains the worktrees.
     "auto_prune_worker_dir"
     ;; nil (omitted) defaults to true.
     "auto_gc_enabled"
     ;; pointer tri-state: nil inherit / true inject / false disable.
     "inject_assigned_skills"
     ;; pointer tri-state (Agent and ProviderSpec): nil inherit / true enable /
     ;; false disable.
     "emits_permission_warning"
     ;; pointer tri-state (ProviderSpec and ProviderPatch): nil default / true
     ;; force / false suppress.
     "accept_startup_dialogs"
     ;; defaults to true; false removes the event hooks.
     "event_hooks"
     ;; defaults to true; false selects the lighter subprocess runtime.
     "attach")
    (secret
     ;; abstract credentials, `$VAR'-shaped.  `UpstreamEnvBinding.api_key' /
     ;; `.auth_token' share these bare names but hold an env-var name; the
     ;; bare-name table cannot separate them, so they are guarded too (which is
     ;; harmless: a bare name is emitted verbatim).
     "api_key"
     "auth_token")
    (gexp
     ;; provider/runtime executable or argv; the `ServiceProcessConfig.command'
     ;; array is incompatible and keeps its array serializer.
     "command"
     ;; overrides the provider command (Agent, AgentOverride, AgentPatch,
     ;; Workspace).
     "start_command"
     ;; ACP-transport command.
     "acp_command"
     ;; shell command that resumes a session.
     "resume_command"
     ;; path to a session-setup script.
     "session_setup_script"
     ;; path to an executable check/command script (PackCommandEntry,
     ;; PackDoctorEntry, LocalDoctorCheck).
     "script"
     ;; path to a remediation script (LocalDoctorCheck, PackDoctorEntry).
     "fix"
     ;; shell command templates run at controller startup / session death.
     "on_boot"
     "on_death"
     ;; shell command templates reporting demand / finding work / routing a
     ;; bead.
     "scale_check"
     "work_query"
     "sling_query"
     ;; condition/scale check command (OrderOverride, PoolOverride); the
     ;; `DoctorConfig.check' array is incompatible and keeps its array
     ;; serializer.
     "check")))

(define (override-category key)
  "Return the override category (a symbol) recorded for KEY, or #f."
  (let loop ((entries %field-overrides))
    (cond
     ((null? entries) #f)
     ((member key (cdar entries)) (caar entries))
     (else (loop (cdr entries))))))


;;;
;;; Field classification.
;;;

(define (property-kind property)
  "Return the base kind of PROPERTY, a JSON schema property, as one of
`(string)', `(boolean)', `(integer)', `(number)', `(array-of-string)',
`(array-of-array-of-string)', `(array-of-ref NAME)', `(map-of-string)',
`(map-of-ref NAME)' or `(ref NAME)'."
  (cond
   ((assoc-ref property "$ref")
    (list 'ref (ref-name (assoc-ref property "$ref"))))
   ((assoc-ref property "oneOf")
    ;; No property in the current schemas uses a union; fall back to the safe
    ;; maybe-string for any future one.
    (list 'string))
   ((assoc-ref property "anyOf")
    (list 'string))
   ((string=? (or (assoc-ref property "type") "") "string") (list 'string))
   ((string=? (or (assoc-ref property "type") "") "boolean") (list 'boolean))
   ((string=? (or (assoc-ref property "type") "") "integer") (list 'integer))
   ((string=? (or (assoc-ref property "type") "") "number") (list 'number))
   ((string=? (or (assoc-ref property "type") "") "array")
    (let ((items (assoc-ref property "items")))
      (cond
       ((assoc-ref items "$ref")
        (list 'array-of-ref (ref-name (assoc-ref items "$ref"))))
       ((equal? (assoc-ref items "type") "string") (list 'array-of-string))
       ((equal? (assoc-ref items "type") "array")
        (list 'array-of-array-of-string))
       (else (error "unsupported array item in schema" property)))))
   ((string=? (or (assoc-ref property "type") "") "object")
    (let ((additional (assoc-ref property "additionalProperties")))
      (cond
       ((assoc-ref additional "$ref")
        (list 'map-of-ref (ref-name (assoc-ref additional "$ref"))))
       ((equal? (assoc-ref additional "type") "string") (list 'map-of-string))
       (else (error "unsupported object in schema" property)))))
   (else (error "unsupported schema property" property))))

(define (base-serializer kind)
  "Return the serializer specification, a list (PROCEDURE [TYPE-NAME]), for a
field of base kind KIND."
  (match kind
    (('string) (list "gascity-records-serialize-maybe-string"))
    (('boolean) (list "gascity-records-serialize-tri-state"))
    (('integer) (list "gascity-records-serialize-maybe-integer"))
    (('number) (list "gascity-records-serialize-maybe-real"))
    (('array-of-string) (list "gascity-records-serialize-list-of-strings"))
    (('array-of-array-of-string)
     (list "gascity-records-serialize-list-of-lists-of-strings"))
    (('array-of-ref ref)
     (list "gascity-records-serialize-subtables" (serializer-name ref)))
    (('map-of-string) (list "gascity-records-serialize-string-map"))
    (('map-of-ref ref)
     (list "gascity-records-serialize-alist-subtables" (serializer-name ref)))
    (('ref ref) (list "gascity-records-serialize-subtable" (serializer-name ref)))
    (else (error "no serializer for kind" kind))))

(define (field-serializer key property)
  "Return the serializer specification for the field KEY with schema
PROPERTY, applying the override table when it is compatible with the base
kind."
  (let* ((kind (property-kind property))
         (category (override-category key)))
    (cond
     ((and (eq? category 'tri-state) (eq? (car kind) 'boolean))
      (list "gascity-records-serialize-tri-state"))
     ((and (eq? category 'secret) (eq? (car kind) 'string))
      (list "gascity-records-serialize-secret"))
     ((and (eq? category 'gexp) (eq? (car kind) 'string))
      (list "gascity-records-serialize-string-or-gexp"))
     (else (base-serializer kind)))))

(define (type-fields definition)
  "Return the fields of DEFINITION as a list of (KEY . PROPERTY), sorted by
KEY."
  (sort (map (lambda (entry) (cons (car entry) (cdr entry)))
             (or (assoc-ref definition "properties") '()))
        (lambda (a b) (string<? (car a) (car b)))))


;;;
;;; Emitting the module.
;;;

(define (field-call key property)
  "Return the Scheme expression string that serializes the field KEY with
schema PROPERTY.  KEY is the TOML key; the kebab-case variable bound by
`match-record' is derived from it."
  (match (field-serializer key property)
    ((procedure)
     (format #f "(~a ~s ~a)" procedure key (field->kebab key)))
    ((procedure ref)
     (format #f "(~a ~s ~a ~a)" procedure key (field->kebab key) ref))
    (else (error "bad serializer specification"))))

(define (emit-record port type definition)
  "Write the record type and its `->toml' procedure for TYPE to PORT."
  (let ((fields (type-fields definition)))
    (format port ";; ~a — from `$defs.~a'.~%~%" type type)
    (format port "(define-record-type* ~a~%" (record-name type))
    (format port "  ~a~%" (constructor-name type))
    (format port "  ~a~%" (maker-name type))
    (format port "  ~a~%" (predicate-name type))
    (for-each
     (lambda (field)
       (format port "  (~a~%   ~a~%   (default 'unset))~%"
               (field->kebab (car field))
               (accessor-name type (car field))))
     fields)
    (format port "  )~%~%")
    (format port "(define (~a config)~%" (serializer-name type))
    (format port "  ~s~%"
            (format #f "Return the ordered list of TOML nodes for the body of CONFIG, a ~a."
                    (record-name type)))
    (format port "  (match-record config ~a~%" (record-name type))
    (format port "    (~{~a~^ ~})~%"
            (map (lambda (field) (field->kebab (car field))) fields))
    (if (null? fields)
        (format port "    '()))~%")
        (begin
          (format port "    (append~%")
          (for-each
           (lambda (field)
             (format port "     ~a~%" (field-call (car field) (cdr field))))
           fields)
          (format port "     '())))~%")))))

(define (emit-helpers port)
  "Write the shared serializer helpers, the literal-secret guard and the
sub-table helpers to PORT."
  (format port "~a" "
;;;
;;; Shared serializer helpers.
;;;

;; Every helper returns an ordered list of TOML IR nodes for one field, or the
;; empty list when the field is absent (`unset'), so the record serializers can
;; append the results in declaration order.  A gexp-valued string is the one
;; place a gexp enters the IR; the emitter splices it between the surrounding
;; quotes.

(define (gascity-records-maybe-string? value)
  \"Return #t if VALUE is a string or `unset'.\"
  (or (eq? value 'unset) (string? value)))

(define (gascity-records-string-or-gexp? value)
  \"Return #t if VALUE is a string, a gexp or `unset'.\"
  (or (eq? value 'unset) (string? value) (gexp? value)))

(define (gascity-records-serialize-maybe-string key value)
  \"Return a field node for KEY with VALUE, or nothing when VALUE is `unset'.\"
  (if (eq? value 'unset)
      '()
      (list (toml-field key value))))

(define (gascity-records-serialize-string-or-gexp key value)
  \"Return a field node for KEY with VALUE, a string or a gexp, or nothing
when VALUE is `unset'.\"
  (cond
   ((eq? value 'unset) '())
   ((or (string? value) (gexp? value)) (list (toml-field key value)))
   (else
    (error \"gascity-records: field must be a string, a gexp or 'unset\" key value))))

(define (gascity-records-serialize-tri-state key value)
  \"Return a boolean field node for KEY, or nothing when VALUE is `unset'.\"
  (cond
   ((eq? value 'unset) '())
   ((boolean? value) (list (toml-field key value)))
   (else
    (error \"gascity-records: tri-state field must be 'unset, #t or #f\" key value))))

(define (gascity-records-serialize-maybe-integer key value)
  \"Return an integer field node for KEY, or nothing when VALUE is `unset'.\"
  (cond
   ((eq? value 'unset) '())
   ((exact-integer? value) (list (toml-field key value)))
   (else
    (error \"gascity-records: integer field must be an exact integer or 'unset\" key value))))

(define (gascity-records-serialize-maybe-real key value)
  \"Return a real field node for KEY, or nothing when VALUE is `unset'.\"
  (cond
   ((eq? value 'unset) '())
   ((real? value) (list (toml-field key value)))
   (else
    (error \"gascity-records: real field must be a real number or 'unset\" key value))))

(define (gascity-records-serialize-list-of-strings key value)
  \"Return an array field node for KEY with the list VALUE, or nothing when
VALUE is `unset'.\"
  (if (eq? value 'unset)
      '()
      (list (toml-field key (apply toml-array value)))))

(define (gascity-records-serialize-list-of-lists-of-strings key value)
  \"Return an array-of-arrays field node for KEY with the list VALUE, or
nothing when VALUE is `unset'.\"
  (if (eq? value 'unset)
      '()
      (list (toml-field key
                        (apply toml-array
                               (map (lambda (row) (apply toml-array row))
                                    value))))))

(define (gascity-records-serialize-string-map key value)
  \"Return an inline-table field node for KEY with the alist VALUE, or nothing
when VALUE is `unset'.\"
  (if (eq? value 'unset)
      '()
      (list (toml-field key (apply toml-inline-table value)))))

(define (gascity-records-serialize-subtable key value proc)
  \"Return a `[KEY]' table node whose body is (PROC VALUE), or nothing when
VALUE is `unset' or has an empty body.\"
  (if (eq? value 'unset)
      '()
      (let ((nodes (proc value)))
        (if (null? nodes) '() (list (apply toml-table key nodes))))))

(define (gascity-records-serialize-subtables key values proc)
  \"Return a `[[KEY]]' array-of-tables node with one element per element of
VALUES, each element the body (PROC VALUE), or nothing when VALUES is `unset'
or empty.\"
  (if (or (eq? values 'unset) (null? values))
      '()
      (list (apply toml-array-of-tables key (map proc values)))))

(define (gascity-records-serialize-alist-subtables key value proc)
  \"Return a `[KEY]' table node holding one `[KEY.NAME]' child table per
(NAME . ITEM) pair of the alist VALUE, each child the body (PROC ITEM), or
nothing when VALUE is `unset' or empty.\"
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
  (list \"sk-ant-\" \"ghp_\" \"gho_\" \"ghu_\" \"ghs_\" \"ghr_\" \"github_pat_\"
        \"xoxb-\" \"xoxp-\" \"xoxa-\" \"xoxr-\" \"xoxs-\" \"glpat-\" \"AIza\" \"AKIA\"))

(define (gascity-records-secret-character? character)
  \"Return #t if CHARACTER may appear in an opaque token.\"
  (or (char-alphabetic? character)
      (char-numeric? character)
      (memv character '(#\\_ #\\+ #\\- #\\=))))

(define (gascity-records-high-entropy-token? string)
  \"Return #t if STRING looks like an opaque credential: long, free of
path/URL/namespace punctuation, and mixing letters and digits.\"
  (and (> (string-length string) 40)
       (string-every gascity-records-secret-character? string)
       (string-any char-numeric? string)
       (string-any char-alphabetic? string)))

(define (gascity-records-literal-secret? value)
  \"Return #t if VALUE, a string, is an obviously literal secret.  A
`NAME=value' assignment is judged on its value part; a value that is a `$VAR'
reference is not a secret.\"
  (and (string? value)
       (let* ((string (string-trim-both value))
              (string (if (string-prefix? \"export \" string)
                          (string-trim (substring string 7))
                          string))
              (equals (string-index string #\\=))
              (string (if equals (substring string (+ equals 1)) string)))
         (and (not (string-null? string))
              (not (string-prefix? \"$\" string))
              (or (any (lambda (prefix) (string-contains string prefix))
                       %gascity-records-secret-prefixes)
                  (gascity-records-high-entropy-token? string))))))

(define (gascity-records-serialize-secret key value)
  \"Return a field node for the secret field KEY with VALUE, or nothing when
VALUE is `unset'.  Raise when VALUE is an obviously literal secret; a `$VAR'
reference or an ordinary env-var name is emitted verbatim.\"
  (cond
   ((eq? value 'unset) '())
   ((gascity-records-literal-secret? value)
    (error \"gascity-records: refusing the literal secret; write a $VAR reference\" key value))
   (else (list (toml-field key value)))))

"))

(define (exported-symbols defs)
  "Return the flat, sorted list of accessor and constructor names to export for
DEFS."
  (sort (apply append
               (map (lambda (definition)
                      (let ((type (car definition)))
                        (cons* (constructor-name type)
                               (predicate-name type)
                               (serializer-name type)
                               (map (lambda (field)
                                      (accessor-name type (car field)))
                                    (type-fields (cdr definition))))))
                    defs))
        string<?))

(define (emit-module port defs)
  "Write the whole generated module for the type definitions DEFS to PORT."
  (define city (read-json (city-schema-path)))
  (define pack (read-json (pack-schema-path)))
  (format port ";;; records.scm --- GENERATED Gas City leaf configuration records~%")
  (format port ";;; Copyright © 2026 Roman Scherer <roman@burningswell.com>~%")
  (format port ";;;~%")
  (format port ";;; This file is part of the r0man Guix channel.~%")
  (format port ";;;~%")
  (format port ";;; This program is free software: you can redistribute it and/or modify it~%")
  (format port ";;; under the terms of the GNU General Public License as published by the~%")
  (format port ";;; Free Software Foundation, either version 3 of the License, or (at your~%")
  (format port ";;; option) any later version.~%")
  (format port ";;;~%")
  (format port ";;; This program is distributed in the hope that it will be useful, but~%")
  (format port ";;; WITHOUT ANY WARRANTY; without even the implied warranty of~%")
  (format port ";;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU~%")
  (format port ";;; General Public License for more details.~%")
  (format port ";;;~%")
  (format port ";;; You should have received a copy of the GNU General Public License along~%")
  (format port ";;; with this program.  If not, see <https://www.gnu.org/licenses/>.~%~%")
  (format port ";;; Commentary:~%")
  (format port ";;;~%")
  (format port ";;; THIS FILE IS GENERATED.  DO NOT EDIT BY HAND.~%")
  (format port ";;;~%")
  (format port ";;; Regenerate with `make generate-gascity-records' and check the~%")
  (format port ";;; committed copy with `make check-gascity-records'.  The generator is~%")
  (format port ";;; scripts/generate-gascity-records.scm; its inputs are the committed~%")
  (format port ";;; copies of the Gas City JSON schemas:~%")
  (format port ";;;~%")
  (format port ";;;   city-schema.json  $id: ~a~%" (assoc-ref city "$id"))
  (format port ";;;                     $schema: ~a~%" (assoc-ref city "$schema"))
  (format port ";;;                     title: ~a~%" (assoc-ref city "title"))
  (format port ";;;   pack-schema.json  $id: ~a~%" (assoc-ref pack "$id"))
  (format port ";;;                     $schema: ~a~%" (assoc-ref pack "$schema"))
  (format port ";;;                     title: ~a~%" (assoc-ref pack "title"))
  (format port ";;;~%")
  (format port ";;; This is a standalone leaf-record surface: it imports only~%")
  (format port ";;; `(guix records)', `(guix gexp)', `(r0man guix toml)',~%")
  (format port ";;; `(srfi srfi-1)' and `(ice-9 match)', and inlines its own~%")
  (format port ";;; serialization, so it never depends on~%")
  (format port ";;; `(r0man guix services gascity)'.  Every field defaults to~%")
  (format port ";;; `unset'; a record's `->toml' returns the ordered list of TOML nodes~%")
  (format port ";;; for its body, in field-declaration order.~%")
  (format port ";;;~%")
  (format port ";;; Code:~%~%")
  (format port "(define-module (r0man guix services gascity records)~%")
  (format port "  #:use-module (guix records)~%")
  (format port "  #:use-module (guix gexp)~%")
  (format port "  #:use-module (r0man guix toml)~%")
  (format port "  #:use-module (srfi srfi-1)~%")
  (format port "  #:use-module (ice-9 match)~%")
  (format port "  #:export (~%")
  (for-each
   (lambda (symbol) (format port "            ~a~%" symbol))
   (exported-symbols defs))
  (format port "            ))~%~%")
  (emit-helpers port)
  (format port "~%")
  (format port ";;;~%")
  (format port ";;; Leaf configuration records (~a types).~%"
          (length defs))
  (format port ";;;~%~%")
  (let loop ((remaining defs) (first? #t))
    (unless (null? remaining)
      (unless first? (format port "~%"))
      (emit-record port (caar remaining) (cdar remaining))
      (loop (cdr remaining) #f))))

(define (main arguments)
  "Generate the records module.  The optional first argument is the output
path; it defaults to the committed module."
  (let* ((output (if (and (pair? (cdr arguments)) (not (string-null? (cadr arguments))))
                     (cadr arguments)
                     %default-output))
         (types (merge-defs)))
    (call-with-output-file output
      (lambda (port) (emit-module port types))
      #:encoding "UTF-8")
    (format (current-error-port)
            "generate-gascity-records: wrote ~a (~a types)~%"
            output (length types))
    (exit 0)))

(main (command-line))
