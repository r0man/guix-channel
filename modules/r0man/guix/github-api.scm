;;; github-api.scm --- Shared GitHub REST API plumbing
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
;;; The two parameters every GitHub REST API client in this channel shares:
;;; the HTTP request procedure (stubbed out by the tests) and the API base
;;; URL (pointed at a mock server by the store-program tests).  Both
;;; (r0man guix services github-actions-vm-mint) and (r0man guix github-app)
;;; use them; the former re-exports them, so its callers are unchanged.
;;;
;;; The module depends on Guile core only, so it can be imported into store
;;; programs with `with-imported-modules'.
;;;
;;; Code:

(define-module (r0man guix github-api)
  #:use-module (web client)
  #:export (%github-api-base
            %http-request))

(define %http-request
  ;; The HTTP request procedure, a parameter so that tests can stub it
  ;; out; defaults to Guile core's 'http-request' (see the design doc
  ;; for why (guix http-client) could not be used: it is GET-only).
  (make-parameter http-request))

(define %github-api-base
  ;; Base URL of the GitHub REST API.  Overridable with the
  ;; GITHUB_API_BASE environment variable, which lets tests point the
  ;; store programs at a mock API server.
  (make-parameter (or (getenv "GITHUB_API_BASE") "https://api.github.com")))

;;; github-api.scm ends here
