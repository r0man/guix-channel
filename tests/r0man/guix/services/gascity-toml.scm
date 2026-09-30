;;; Unit tests for the TOML intermediate representation and emitter in
;;; (r0man guix toml): string escaping, bare vs quoted keys, value
;;; formatting, arrays, inline tables, normal nested tables, arrays of
;;; tables, a child table under an array element, the sibling ordering rule,
;;; a complete multi-section golden document, duplicate-scalar-key
;;; rejection, and gexp splicing.
;;;
;;; `toml-document->string' returns a gexp, so the golden tests reduce it to a
;;; plain string with `eval-gexp' -- the same trick Guix's own
;;; tests/services/configuration.scm uses -- before comparing.

(define-module (test-r0man-guix-services-gascity-toml)
  #:use-module (guix gexp)
  #:use-module (r0man guix toml)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

(define (eval-gexp x)
  "Get the serialized config as a string."
  (eval (gexp->approximate-sexp x)
        (current-module)))

(define (render document)
  "Return the TOML text that DOCUMENT lowers to."
  (eval-gexp (toml-document->string document)))

(test-begin "gascity-toml")

(test-group "string escaping"
  (test-equal "quotes, backslashes, newlines, tabs and control chars"
    "k = \"a\\\"b\\\\c\\nd\\te\\b\\u0001\"\n"
    (render
     (toml-document
      (toml-field "k" (string-append "a\"b\\c\nd\te"
                                     (string (integer->char 8))
                                     (string (integer->char 1))))))))

(test-group "key quoting"
  (test-equal "bare keys are left unquoted, others become basic strings"
    (string-append "simple = 1\n"
                   "with-dash = 2\n"
                   "with_underscore = 3\n"
                   "123 = 4\n"
                   "\"with.dot\" = 5\n"
                   "\"with space\" = 6\n")
    (render
     (toml-document
      (toml-field "simple" 1)
      (toml-field "with-dash" 2)
      (toml-field "with_underscore" 3)
      (toml-field "123" 4)
      (toml-field "with.dot" 5)
      (toml-field "with space" 6)))))

(test-group "value formatting"
  (test-equal "integers are decimal and floats always carry a decimal point"
    (string-append "i = 42\n"
                   "neg = -7\n"
                   "f = 1.5\n"
                   "whole = 2.0\n"
                   "big = 1.0e10\n")
    (render
     (toml-document
      (toml-field "i" 42)
      (toml-field "neg" -7)
      (toml-field "f" 1.5)
      (toml-field "whole" 2.0)
      (toml-field "big" 1e10))))

  (test-equal "booleans"
    "t = true\nf = false\n"
    (render
     (toml-document (toml-field "t" #t) (toml-field "f" #f)))))

(test-group "arrays"
  (test-equal "empty, scalar, string and nested arrays"
    (string-append "empty = []\n"
                   "nums = [1, 2, 3]\n"
                   "strs = [\"x\", \"y\"]\n"
                   "nested = [[1], [2]]\n")
    (render
     (toml-document
      (toml-field "empty" (toml-array))
      (toml-field "nums" (toml-array 1 2 3))
      (toml-field "strs" (toml-array "x" "y"))
      (toml-field "nested" (toml-array (toml-array 1) (toml-array 2)))))))

(test-group "inline tables"
  (test-equal "empty, populated and quoted keys"
    (string-append "empty = {}\n"
                   "simple = { a = 1, b = \"x\" }\n"
                   "dotted = { \"a.b\" = 1 }\n")
    (render
     (toml-document
      (toml-field "empty" (toml-inline-table))
      (toml-field "simple" (toml-inline-table (cons "a" 1) (cons "b" "x")))
      (toml-field "dotted" (toml-inline-table (cons "a.b" 1)))))))

(test-group "nested normal tables"
  (test-equal "fields precede child tables and a header is preceded by a blank line"
    (string-append "root = \"r\"\n"
                   "\n"
                   "[workspace]\n"
                   "name = \"w\"\n"
                   "\n"
                   "[workspace.nested]\n"
                   "k = 1\n")
    (render
     (toml-document
      (toml-field "root" "r")
      (toml-table "workspace"
                  (toml-field "name" "w")
                  (toml-table "nested" (toml-field "k" 1)))))))

(test-group "arrays of tables"
  (test-equal "a single element"
    (string-append "[[agent]]\n"
                   "name = \"a\"\n")
    (render
     (toml-document
      (toml-array-of-tables "agent" (list (toml-field "name" "a"))))))

  (test-equal "several elements each get their own header"
    (string-append "[[agent]]\n"
                   "name = \"a\"\n"
                   "\n"
                   "[[agent]]\n"
                   "name = \"b\"\n")
    (render
     (toml-document
      (toml-array-of-tables "agent"
                            (list (toml-field "name" "a"))
                            (list (toml-field "name" "b"))))))

  (test-equal "a child table attaches to the last array element"
    (string-append "[[rigs]]\n"
                   "path = \"/x\"\n"
                   "\n"
                   "[rigs.patches]\n"
                   "k = 1\n"
                   "\n"
                   "[[rigs]]\n"
                   "path = \"/y\"\n")
    (render
     (toml-document
      (toml-array-of-tables
       "rigs"
       (list (toml-field "path" "/x")
             (toml-table "patches" (toml-field "k" 1)))
       (list (toml-field "path" "/y")))))))

(test-group "sibling ordering"
  (test-equal "fields before child tables before arrays of tables"
    (string-append "ff = 1\n"
                   "\n"
                   "[tt]\n"
                   "x = 1\n"
                   "\n"
                   "[[aa]]\n"
                   "y = 1\n")
    (render
     (toml-document
      (toml-array-of-tables "aa" (list (toml-field "y" 1)))
      (toml-table "tt" (toml-field "x" 1))
      (toml-field "ff" 1)))))

(test-group "symbol keys"
  (test-equal "field, table, array-of-tables and inline-table keys accept symbols"
    (string-append "feature_flag = \"true\"\n"
                   "\n"
                   "[experimental]\n"
                   "k = 1\n"
                   "\n"
                   "[[agent]]\n"
                   "name = \"a\"\n")
    (render
     (toml-document
      (toml-field 'feature_flag "true")
      (toml-table 'experimental (toml-field 'k 1))
      (toml-array-of-tables 'agent (list (toml-field 'name "a"))))))

  (test-equal "inline-table keys accept symbols"
    "env = { FOO = \"bar\" }\n"
    (render
     (toml-document
      (toml-field 'env (toml-inline-table (cons 'FOO "bar"))))))

  (test-equal "symbol and string keys are the same key for duplicate detection"
    "a = 1\n"
    (render (toml-document (toml-field 'a 1)))))

(test-group "golden document"
  (test-equal "a complete multi-section document"
    (string-append "name = \"burningswell\"\n"
                   "enabled = true\n"
                   "\n"
                   "[workspace]\n"
                   "provider = \"claude\"\n"
                   "env = { FOO = \"bar\", HOME = \"/home/x\" }\n"
                   "\n"
                   "[workspace.nested]\n"
                   "k = 1\n"
                   "\n"
                   "[providers]\n"
                   "\n"
                   "[providers.claude]\n"
                   "base = \"anthropic\"\n"
                   "\n"
                   "[[agent]]\n"
                   "name = \"a\"\n"
                   "model = \"opus\"\n"
                   "\n"
                   "[[agent]]\n"
                   "name = \"b\"\n")
    (render
     (toml-document
      (toml-field "name" "burningswell")
      (toml-field "enabled" #t)
      (toml-table "workspace"
                  (toml-field "provider" "claude")
                  (toml-field "env" (toml-inline-table
                                     (cons "FOO" "bar")
                                     (cons "HOME" "/home/x")))
                  (toml-table "nested" (toml-field "k" 1)))
      (toml-table "providers"
                  (toml-table "claude" (toml-field "base" "anthropic")))
      (toml-array-of-tables "agent"
                            (list (toml-field "name" "a")
                                  (toml-field "model" "opus"))
                            (list (toml-field "name" "b")))))))

(test-group "duplicate scalar keys"
  (test-assert "duplicate scalar keys are rejected"
    (guard (condition (#t #t))
      (toml-document->string
       (toml-document (toml-field "a" 1) (toml-field "a" 2)))
      #f))

  (test-assert "duplicate keys are accepted when spelled differently"
    (guard (condition (#t #f))
      (toml-document->string
       (toml-document (toml-field "a" 1) (toml-field "b" 2)))
      #t)))

(test-group "gexps"
  (test-assert "toml-document->string always returns a gexp"
    (gexp? (toml-document->string (toml-document (toml-field "a" 1)))))

  (test-equal "an empty document lowers to the empty string"
    ""
    (render (toml-document)))

  (test-equal "a gexp-valued string field is spliced between the quotes"
    "cmd = \"splice\"\n"
    (render
     (toml-document
      (toml-field "cmd" #~(string-append "sp" "lice"))))))

(test-end "gascity-toml")
