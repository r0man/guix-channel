;;; script.scm --- helpers shared by the Gas City activation programs
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
;;; The procedures here run in two places: at configuration time (the service
;;; and the unit tests call them directly) and inside the activation and
;;; provision programs the service generates.  Those programs cannot import
;;; `(r0man guix services gascity)' -- it drags in the whole Guix package
;;; graph -- so the code they share with it lives in this module, which
;;; depends on Guile core and SRFI-1/13 only.  Each program adds it with
;;; `with-imported-modules' and imports it with `use-modules'.
;;;
;;; Nothing here knows about a supervisor instance: DIRECTORY is a city root,
;;; GC the `gc' executable and LOG a `format'-style procedure supplied by the
;;; caller.
;;;
;;; The `cities.toml' code owns exactly the `[[cities]]' set of the registry
;;; and preserves the raw text of every other section, because `gc' keeps
;;; `[[rigs]]' and `[[pending_city_requests]]' in the same file and rewrites
;;; it under an flock (design doc §16.3).
;;;
;;; The reload/import helpers never fail their caller: the generated files
;;; are correct on disk either way, and `gc' applies them on its next reload
;;; or start.

(define-module (r0man guix services gascity script)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (gascity-cities-toml-merge
            gascity-soft-reload
            gascity-sync-packs))


;;;
;;; `cities.toml' read-merge-write.
;;;

(define (gascity-toml-table-header line)
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
  "Return the sections of TEXT as a list of (HEADER . LINES): each HEADER is
(KIND . KEY) or #f for the preamble, and LINES are the raw lines of the
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
            (list (string-append "name = " (gascity-toml-quote-string name)))
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
  "Return LINES without the blank lines that precede the first and follow the
last non-blank line."
  (reverse
   (gascity-strip-leading-blank-lines
    (reverse (gascity-strip-leading-blank-lines lines)))))

(define (gascity-cities-toml-merge existing cities)
  "Return new `cities.toml' text: replace the `[[cities]]' set of EXISTING (a
string, or #f for an empty file) with CITIES while preserving the raw text of
every other section, in particular `[[rigs]]' and
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
       "\n")))))


;;;
;;; Bringing a running city up to date.
;;;

(define (gascity-sync-packs gc directory log)
  "Make the installed imports of the city in DIRECTORY match its declaration:
run `gc import install' when `gc import check' reports drift.  Return true
when the imports are in sync."
  (or (zero? (system* gc "import" "check" "--city" directory))
      (begin
        (log "imports of ~a differ from the declaration; running gc \
import install" directory)
        (or (zero? (system* gc "import" "install" "--city" directory))
            (begin
              (log "gc import install failed for ~a; the city keeps its \
previous configuration until the next attempt" directory)
              #f)))))

(define (gascity-soft-reload gc directory log)
  "Ask the controller of the city in DIRECTORY to reload its configuration
and to accept the config drift of its open sessions instead of draining them.
Return true when the controller accepted the request."
  ;; `--async' returns once the controller accepted the request (gc refuses
  ;; it together with `--timeout').
  (or (zero? (system* gc "reload" "--soft" "--async" directory))
      (begin
        (log "gc reload --soft of ~a did not complete (no running \
controller?); it picks the change up when it next loads" directory)
        #f)))
