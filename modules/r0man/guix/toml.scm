;;; toml.scm --- a small TOML intermediate representation and emitter
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
;;; This module provides a small, self-contained TOML intermediate
;;; representation (IR) and an emitter that compiles an ordered document into
;;; a Guix gexp.
;;;
;;; TOML is order-sensitive in a way that plain string concatenation cannot
;;; express: a table header may only be written once, keys must precede the
;;; sub-tables of their table, and a `[a.sub]' header written after `[[a]]'
;;; attaches to the *last* element of the `a' array.  The emitter here owns
;;; that ordering so that record serializers can stay simple.
;;;
;;; The value grammar is:
;;;
;;;   string | boolean | exact integer | inexact real | gexp
;;;     | (toml-array VALUE ...)
;;;     | (toml-inline-table (KEY . VALUE) ...)
;;;
;;; A gexp is a first-class value kind: it is spliced verbatim between the
;;; quotes of the surrounding basic string, so operators can embed store paths
;;; and file-like values.  Only literal fragments are escaped, and they are
;;; escaped at build time; a spliced gexp is inserted as-is and the operator is
;;; responsible for content that needs no further escaping.
;;;
;;; A node (in order) is one of:
;;;
;;;   (toml-field KEY VALUE)              -> KEY = VALUE
;;;   (toml-table KEY NODE ...)           -> [KEY] then NODE ...
;;;   (toml-array-of-tables KEY ELEMENT ...) -> [[KEY]] per ELEMENT
;;;          where ELEMENT is a list of nodes
;;;
;;; A document is an ordered list of nodes.  `toml-document->string' returns a
;;; gexp that lowers to the TOML text; there is no plain-string-only sibling.
;;;
;;; The canonical formatting is pinned by the golden tests in
;;; tests/r0man/guix/services/gascity-toml.scm:
;;;
;;;   - every emitted line ends with a single newline, including the last;
;;;   - within a table the fields are written first, one per line, with no
;;;     blank lines between them;
;;;   - a table header (`[P.key]' or `[[P.key]]') is preceded by a blank line
;;;     unless it is the first line of the document, which separates sibling
;;;     top-level nodes;
;;;   - a header is immediately followed (on the next line) by its own fields;
;;;   - a header is emitted for every declared table and for every array
;;;     element, even when the table has no direct fields;
;;;   - sibling nodes are grouped by kind, each group in first-appearance
;;;     order: the fields first, then the normal child tables, then the arrays
;;;     of tables.
;;;
;;; Code:

(define-module (r0man guix toml)
  #:use-module (guix gexp)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (toml-field
            toml-field?
            toml-field-key
            toml-field-value
            toml-table
            toml-table?
            toml-table-key
            toml-table-nodes
            toml-array-of-tables
            toml-array-of-tables?
            toml-array-of-tables-key
            toml-array-of-tables-elements
            toml-inline-table
            toml-inline-table?
            toml-array
            toml-array?
            toml-document
            toml-document?
            toml-document->string))


;;;
;;; Records.
;;;

;; Each node and value is a tagged list so that this module stays
;; dependency-free and only imports the modules listed above.  The tag is the
;; first element, which is why TOML lists are never bare Scheme lists: they are
;; always built with `toml-array' or `toml-inline-table'.

;; A key is either a string or a symbol; it is normalized to a string at the
;; constructor boundary so that both spellings work everywhere a key is
;; accepted and the emitter and key-printing helpers only ever see strings.
(define (toml-key key)
  "Return KEY normalized to a string: a symbol becomes its name, a string
passes through unchanged."
  (if (symbol? key) (symbol->string key) key))

(define (toml-field key value)
  "Return a TOML field node for KEY with VALUE."
  (list 'toml-field (toml-key key) value))

(define (toml-field? thing)
  "Return #t if THING is a TOML field node."
  (and (pair? thing) (eq? (car thing) 'toml-field)))

(define (toml-field-key field)
  "Return the key of FIELD."
  (list-ref field 1))

(define (toml-field-value field)
  "Return the value of FIELD."
  (list-ref field 2))

(define (toml-table key . nodes)
  "Return a TOML table node named KEY containing NODES."
  (cons 'toml-table (cons (toml-key key) nodes)))

(define (toml-table? thing)
  "Return #t if THING is a TOML table node."
  (and (pair? thing) (eq? (car thing) 'toml-table)))

(define (toml-table-key table)
  "Return the key of TABLE."
  (cadr table))

(define (toml-table-nodes table)
  "Return the nodes of TABLE."
  (cddr table))

(define (toml-array-of-tables key . elements)
  "Return a TOML array-of-tables node named KEY.  Each of ELEMENTS is a list
of nodes describing one table of the array."
  (cons 'toml-array-of-tables (cons (toml-key key) elements)))

(define (toml-array-of-tables? thing)
  "Return #t if THING is a TOML array-of-tables node."
  (and (pair? thing) (eq? (car thing) 'toml-array-of-tables)))

(define (toml-array-of-tables-key array)
  "Return the key of ARRAY."
  (cadr array))

(define (toml-array-of-tables-elements array)
  "Return the elements of ARRAY, each a list of nodes."
  (cddr array))

(define (toml-inline-table . pairs)
  "Return a TOML inline table value built from PAIRS, each a (KEY . VALUE)
pair."
  (cons 'toml-inline-table
        (map (lambda (pair) (cons (toml-key (car pair)) (cdr pair))) pairs)))

(define (toml-inline-table? thing)
  "Return #t if THING is a TOML inline table value."
  (and (pair? thing) (eq? (car thing) 'toml-inline-table)))

(define (toml-inline-table-pairs table)
  "Return the (KEY . VALUE) pairs of TABLE."
  (cdr table))

(define (toml-array . values)
  "Return a TOML array value built from VALUES."
  (cons 'toml-array values))

(define (toml-array? thing)
  "Return #t if THING is a TOML array value."
  (and (pair? thing) (eq? (car thing) 'toml-array)))

(define (toml-array-values array)
  "Return the values of ARRAY."
  (cdr array))

(define (toml-document . nodes)
  "Return a TOML document containing NODES, in order."
  (cons 'toml-document nodes))

(define (toml-document? thing)
  "Return #t if THING is a TOML document."
  (and (pair? thing) (eq? (car thing) 'toml-document)))

(define (toml-document-nodes document)
  "Return the ordered nodes of DOCUMENT."
  (cdr document))


;;;
;;; Values.
;;;

(define (escape-character character)
  "Return the escaped TOML representation of CHARACTER, a control character,
backslash or double quote."
  (case character
    ((#\\) "\\\\")
    ((#\") "\\\"")
    ((#\x08) "\\b")
    ((#\x09) "\\t")
    ((#\x0a) "\\n")
    ((#\x0c) "\\f")
    ((#\x0d) "\\r")
    (else
     (let ((code (char->integer character)))
       (if (or (< code #x20) (= code #x7f))
           (format #f "\\u~4,'0X" code)
           (string character))))))

(define (escape-string string)
  "Return STRING with the backslashes, double quotes and control characters
escaped for a TOML basic string."
  (if (= (string-length string) 0)
      ""
      (apply string-append (map escape-character (string->list string)))))

(define (bare-key-character? character)
  "Return #t if CHARACTER may appear in an unquoted TOML key."
  (or (and (char>=? character #\0) (char<=? character #\9))
      (and (char>=? character #\A) (char<=? character #\Z))
      (and (char>=? character #\a) (char<=? character #\z))
      (eq? character #\-)
      (eq? character #\_)))

(define (bare-key? key)
  "Return #t if KEY can be emitted without quoting, i.e. it is a non-empty
string of `[A-Za-z0-9_-]' characters."
  (and (string? key)
       (> (string-length key) 0)
       (let loop ((index 0))
         (or (= index (string-length key))
             (and (bare-key-character? (string-ref key index))
                  (loop (+ index 1)))))))

(define (quote-key key)
  "Return KEY, quoted as a TOML basic string unless it is a bare key."
  (if (bare-key? key)
      key
      (string-append "\"" (escape-string key) "\"")))

(define (string-has-character? string character)
  "Return #t if STRING contains CHARACTER."
  (let loop ((index 0))
    (and (< index (string-length string))
         (or (eq? (string-ref string index) character)
             (loop (+ index 1))))))

(define (join-fragments fragments)
  "Concatenate FRAGMENTS, each either a plain string or a gexp, into a single
fragment.  The result is always a gexp: the fragments are spliced one at a
time rather than as one list, because `gexp->approximate-sexp' (used by the
tests) only recurses into a reference that is itself a gexp, not into a list
of gexps spliced all at once."
  (fold (lambda (fragment result)
          #~(string-append #$result #$fragment))
        #~""
        fragments))

(define (separate-fragments separator fragments)
  "Return FRAGMENTS with SEPARATOR inserted between consecutive elements."
  (match fragments
    (() '())
    ((first . rest)
     (append (list first)
             (append-map (lambda (fragment) (list separator fragment)) rest)))))

(define (toml-value->string value)
  "Return VALUE serialized as a TOML value fragment (a string or a gexp)."
  (cond
   ((gexp? value)
    ;; The escaped boundary of the emitter: the spliced gexp is inserted
    ;; verbatim between the surrounding quotes, with no further escaping.
    #~(string-append "\"" #$value "\""))
   ((string? value)
    (string-append "\"" (escape-string value) "\""))
   ((boolean? value)
    (if value "true" "false"))
   ((exact-integer? value)
    (number->string value))
   ((and (real? value) (inexact? value))
    (let ((string (number->string value)))
      ;; A TOML float must contain a decimal point or an exponent.
      (if (or (string-has-character? string #\.)
              (string-has-character? string #\e)
              (string-has-character? string #\E))
          string
          (string-append string ".0"))))
   ((toml-array? value)
    (toml-array->string value))
   ((toml-inline-table? value)
    (toml-inline-table->string value))
   (else
    (error (format #f "toml: unsupported value ~s" value)))))

(define (toml-array->string array)
  "Return ARRAY serialized as a TOML array fragment."
  (join-fragments
   (append (list "[")
           (separate-fragments ", " (map toml-value->string (toml-array-values array)))
           (list "]"))))

(define (toml-inline-table-pair->string pair)
  "Return PAIR, a (KEY . VALUE) pair, serialized as a TOML inline-table body."
  (join-fragments
   (list (quote-key (car pair))
         " = "
         (toml-value->string (cdr pair)))))

(define (toml-inline-table->string table)
  "Return TABLE serialized as a TOML inline table fragment."
  (let ((pairs (toml-inline-table-pairs table)))
    (if (null? pairs)
        "{}"
        (join-fragments
         (append (list "{ ")
                 (separate-fragments ", " (map toml-inline-table-pair->string pairs))
                 (list " }"))))))


;;;
;;; Emitter.
;;;

(define (path->string path)
  "Return PATH, a list of keys from the root, quoted and joined with dots."
  (if (null? path)
      ""
      (fold (lambda (key result)
              (if (= (string-length result) 0)
                  (quote-key key)
                  (string-append result "." (quote-key key))))
            ""
            path)))

(define (duplicate-location path)
  "Return a human-readable description of the table at PATH."
  (if (null? path)
      "the root table"
      (string-append "table [" (path->string path) "]")))

(define (check-field-keys path fields)
  "Raise an error if FIELDS contains two fields with the same key.  This
catches duplicate scalar keys, a programmer error.  The `extra-config'
precedence merge is a separate pre-emission pass and never happens here."
  (let loop ((seen '()) (fields fields))
    (match fields
      (() #t)
      ((field . rest)
       (let ((key (toml-field-key field)))
         (if (member key seen)
             (error (format #f "toml: duplicate scalar key ~s in ~a"
                            key (duplicate-location path)))
             (loop (cons key seen) rest)))))))

(define (toml-field->line field)
  "Return FIELD serialized as a `key = value' line, without a newline."
  (join-fragments
   (list (quote-key (toml-field-key field))
         " = "
         (toml-value->string (toml-field-value field)))))

(define (emit-header header lines)
  "Cons HEADER onto the reversed LINES, preceded by a blank line unless LINES
is empty.  Writing the blank line before every header is what separates the
sibling top-level nodes (and the tables of an array) from one another."
  (if (null? lines)
      (cons header lines)
      (cons header (cons 'blank lines))))

(define (emit-table path nodes lines)
  "Emit the body of the table at PATH containing NODES onto the reversed
LINES and return the updated reversed list.  The table's own header is the
caller's responsibility.

The body is grouped by kind, each group in first-appearance order: the fields
first, then the normal child tables (as `[P.key]'), then the arrays of tables
(as `[[P.key]]').  Each array element emits its own header, then its fields,
then its child tables (which correctly attach to the last element)."
  (let ((fields (filter toml-field? nodes))
        (tables (filter toml-table? nodes))
        (arrays (filter toml-array-of-tables? nodes)))
    (check-field-keys path fields)
    (let* ((lines (fold (lambda (field lines)
                          (cons (toml-field->line field) lines))
                        lines fields))
           (lines (let tables-loop ((tables tables) (lines lines))
                    (match tables
                      (() lines)
                      ((table . rest)
                       (let* ((child (append path (list (toml-table-key table))))
                              (lines (emit-header
                                      (string-append "[" (path->string child) "]")
                                      lines))
                              (lines (emit-table child (toml-table-nodes table)
                                                 lines)))
                         (tables-loop rest lines))))))
           (lines (let arrays-loop ((arrays arrays) (lines lines))
                    (match arrays
                      (() lines)
                      ((array . rest)
                       (let* ((child (append path
                                             (list (toml-array-of-tables-key array))))
                              (lines (let elements-loop
                                         ((elements (toml-array-of-tables-elements array))
                                          (lines lines))
                                       (match elements
                                         (() lines)
                                         ((element . element-rest)
                                          (let* ((lines
                                                  (emit-header
                                                   (string-append
                                                    "[[" (path->string child) "]]")
                                                   lines))
                                                 (lines (emit-table child element lines)))
                                            (elements-loop element-rest lines)))))))
                         (arrays-loop rest lines)))))))
      lines)))

(define (toml-document->string document)
  "Return a gexp that lowers to the TOML text of DOCUMENT, a <toml-document>.

The document is emitted by a single depth-first walk.  Every line ends with a
newline and the last line is no exception; sibling top-level nodes are
separated by a blank line.  An empty document lowers to the empty string."
  (unless (toml-document? document)
    (error (format #f "toml: not a TOML document: ~s" document)))
  (let* ((lines (emit-table '() (toml-document-nodes document) '()))
         (chunks (map (lambda (line)
                        (if (eq? line 'blank)
                            "\n"
                            #~(string-append #$line "\n")))
                      (reverse lines))))
    (join-fragments chunks)))
