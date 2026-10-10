;;; github-token.scm --- Consume a GitHub token file from git and gh
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
;;; The consumer side of a non-personal GitHub identity: a token file (a
;;; GitHub App installation token minted by `github-app-token-service-type',
;;; or a machine user's classic PAT, for example a sops-guix secret) is turned
;;; into credentials for git and gh.
;;;
;;;   - `github-token-credential-helper' is a git credential helper that
;;;     answers `get' for https://github.com only, with the token as the
;;;     password of `x-access-token';
;;;   - `github-token-gh' wraps gh so it reads `GH_TOKEN' from the file;
;;;   - `github-git-config' is a global git configuration with the commit
;;;     identity, the credential helper and `insteadOf' rewrites that turn
;;;     SSH remotes into HTTPS ones (the service user has no SSH key).
;;;
;;; The token is always read from the file at use time, never embedded in
;;; the store.  When a sidecar `<token-file>.expires_at' exists (the minter
;;; writes one) and it lies in the past, the helper refuses with a clear
;;; "stale token" error instead of handing git a credential GitHub rejects.
;;;
;;; This module deliberately uses neither gnutls nor the Guix package graph
;;; beyond `bash' and `git', so it can be used from both system and Home
;;; configurations.
;;;
;;; Code:

(define-module (r0man guix services github-token)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (github-token-credential-forms
            github-token-credential-helper
            github-token-gh
            github-git-config))

;;;
;;; Credential helper.
;;;

(define github-token-credential-forms
  ;; The credential helper's logic, as forms evaluated both inside the store
  ;; program and by the unit tests (like `gascity-cities-toml-merge-forms').
  ;; It needs Guile core plus (ice-9 rdelim), (ice-9 textual-ports),
  ;; (srfi srfi-1), (srfi srfi-13) and (srfi srfi-19).
  '((define (github-token-read-attributes port)
      ;; Read git's `key=value' lines up to a blank line or end of file.
      (let loop ((attributes '()))
        (let ((line (read-line port)))
          (if (or (eof-object? line)
                  (string-null? (string-trim-both line)))
              (reverse attributes)
              (let ((index (string-index line #\=)))
                (loop (if index
                          (cons (cons (substring line 0 index)
                                      (substring line (+ index 1)))
                                attributes)
                          attributes)))))))

    (define (github-token-parse-expires-at string)
      ;; Return the UTC seconds of STRING, an ISO-8601 time such as
      ;; `2026-10-08T12:34:56Z' (fractional seconds and a `+00:00'
      ;; suffix are tolerated) or plain epoch seconds, or #f.
      (let ((string (string-trim-both string)))
        (cond
         ((string-null? string) #f)
         ((string-every char-numeric? string) (string->number string))
         ((< (string-length string) 19) #f)
         (else
          (let* ((utc? (let ((rest (substring string 19)))
                         (or (string-null? rest)
                             (string-suffix? "Z" rest)
                             (string-suffix? "+00:00" rest)
                             (string-suffix? "+0000" rest))))
                 (date (false-if-exception
                        (string->date (substring string 0 19)
                                      "~Y-~m-~dT~H:~M:~S"))))
            (and utc? date
                 (time-second
                  (date->time-utc
                   (make-date 0 (date-second date) (date-minute date)
                              (date-hour date) (date-day date)
                              (date-month date) (date-year date)
                              0)))))))))

    (define (github-token-read-file file)
      (and (file-exists? file)
           (string-trim-both (call-with-input-file file get-string-all))))

    (define (github-token-credential-answer action input token-file now
                                            output error)
      ;; Answer the git credential ACTION reading the request from INPUT and
      ;; writing the answer to OUTPUT, diagnostics to ERROR.  NOW is the
      ;; current time in UTC seconds.  Return the exit code.
      (if (not (string=? action "get"))
          ;; `store' and `erase': the token file is owned by its minter.
          0
          (let ((attributes (github-token-read-attributes input)))
            (define (attribute key)
              (let ((entry (assoc key attributes)))
                (and entry (cdr entry))))
            (if (not (and (equal? (attribute "protocol") "https")
                          (equal? (attribute "host") "github.com")))
                ;; Stay silent: git tries its other helpers, if any.
                0
                (let* ((sidecar (string-append token-file ".expires_at"))
                       (expires (github-token-read-file sidecar))
                       (expires-at (and expires
                                        (github-token-parse-expires-at
                                         expires)))
                       (token (github-token-read-file token-file)))
                  (cond
                   ((and expires (not expires-at))
                    (format error "github-token: cannot parse ~a: ~s~%"
                            sidecar expires)
                    1)
                   ((and expires-at (<= expires-at now))
                    (format error "github-token: stale token: ~a expired at \
~a; is the token minter running?~%"
                            token-file expires)
                    1)
                   ((or (not token) (string-null? token))
                    (format error "github-token: no token in ~a~%"
                            token-file)
                    1)
                   (else
                    (format output "username=x-access-token~%")
                    (format output "password=~a~%" token)
                    0)))))))))

(define (github-token-credential-helper token-file)
  "Return a git credential helper, as a program-file, that answers `get' for
`protocol=https' and `host=github.com' only, with `username=x-access-token'
and the trimmed contents of TOKEN-FILE as the password.  It ignores every
other host and protocol, does nothing on `store' and `erase', and fails with a
\"stale token\" error when the sidecar `<TOKEN-FILE>.expires_at' lies in the
past."
  (program-file
   "github-token-credential-helper"
   #~(begin
       (use-modules (ice-9 rdelim)
                    (ice-9 textual-ports)
                    (srfi srfi-1)
                    (srfi srfi-13)
                    (srfi srfi-19))
       (for-each (lambda (form)
                   (primitive-eval form))
                 '#$github-token-credential-forms)
       (let ((arguments (cdr (command-line))))
         (exit (github-token-credential-answer
                (if (null? arguments) "" (car arguments))
                (current-input-port)
                #$token-file
                (time-second (current-time time-utc))
                (current-output-port)
                (current-error-port)))))))

;;;
;;; gh wrapper.
;;;

(define (github-token-gh token-file gh)
  "Return a directory, as a file-union, with a `bin/gh' that sets GH_TOKEN
from TOKEN-FILE and execs the real gh of GH, the github-cli package or
another file-like with a `bin/gh'.  Put it *before* github-cli in a gascity
supervisor's `packages': PATH is first-wins, and agents only see the
supervisor's `packages', never a Home profile."
  (file-union
   "github-token-gh"
   `(("bin/gh"
      ,(program-file
        "gh"
        #~(begin
            (use-modules (ice-9 textual-ports))
            (let ((token (false-if-exception
                          (string-trim-both
                           (call-with-input-file #$token-file
                             get-string-all)))))
              (if (and token (not (string-null? token)))
                  (setenv "GH_TOKEN" token)
                  (format (current-error-port)
                          "gh: cannot read a token from ~a~%" #$token-file)))
            (apply execl #$(file-append gh "/bin/gh")
                   (command-line))))))))

;;;
;;; git configuration.
;;;

(define (github-git-config-quote value)
  "Return VALUE, a string, as a quoted git configuration value."
  (string-append
   "\""
   (string-concatenate
    (map (match-lambda
           (#\" "\\\"")
           (#\\ "\\\\")
           (#\newline "\\n")
           (character (string character)))
         (string->list value)))
   "\""))

(define* (github-git-config #:key name email token-file
                            (credential-helper
                             (and token-file
                                  (github-token-credential-helper token-file))))
  "Return a global git configuration, as a file-like, for the commit identity
NAME and EMAIL that authenticates to https://github.com with
CREDENTIAL-HELPER (by default `github-token-credential-helper' of
TOKEN-FILE), rewrites SSH GitHub remotes to HTTPS, and sets
`push.autoSetupRemote'.  Use it as a gascity supervisor's `git-config' (it
becomes GIT_CONFIG_GLOBAL, a read-only store file agents cannot replace) or as
the Home `git/config'."
  (unless credential-helper
    (error "github-git-config: either #:token-file or #:credential-helper \
is required"))
  (mixed-text-file
   "github-git-config"
   "[user]\n"
   "\tname = " (github-git-config-quote name) "\n"
   "\temail = " (github-git-config-quote email) "\n"
   "[credential \"https://github.com\"]\n"
   "\thelper = " credential-helper "\n"
   "[url \"https://github.com/\"]\n"
   "\tinsteadOf = git@github.com:\n"
   "\tinsteadOf = ssh://git@github.com/\n"
   "[push]\n"
   "\tautoSetupRemote = true\n"))

;;; github-token.scm ends here
