;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (r0man guix packages golang-dolthub)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;;; Commentary:
;;;
;;; DoltHub Go packages for the Dolt version-controlled database system.
;;; Dolt is a SQL database that you can fork, clone, branch, merge, push,
;;; and pull just like a Git repository.
;;;
;;; Code:

(define-public go-github-com-dolthub-fslock
  (package
    (name "go-github-com-dolthub-fslock")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/fslock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bhpa2krf478rn6mqswrq3qsbw66afcyihvw7xnl5i2bgni124dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/fslock"
      ;; Tests require gopkg.in/check.v1
      #:tests? #f))
    (home-page "https://github.com/dolthub/fslock")
    (synopsis "File-based mutex lock implementation")
    (description
     "This package provides a simple file-based locking mechanism for
coordinating processes accessing the same resources.")
    (license license:asl2.0)))

(define-public go-github-com-dolthub-maphash
  (package
    (name "go-github-com-dolthub-maphash")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/maphash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ch81m7r3zlrqg4p41af1zklan4h9kigj3wrfyjsjsl3dsi0r1ax"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/maphash"
      ;; Tests require github.com/stretchr/testify
      #:tests? #f))
    (home-page "https://github.com/dolthub/maphash")
    (synopsis "Wrapper around Go's maphash package")
    (description
     "This package provides hash functions compatible with Go's map
implementation using the standard library's maphash package.")
    (license license:asl2.0)))

(define-public go-github-com-dolthub-swiss
  (package
    (name "go-github-com-dolthub-swiss")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/swiss")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sm5lbx5ywpqvv6hrgy12davrhf9aqkhqxgjcy16ysl3dk0pxi99"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/swiss"
      ;; Tests require github.com/stretchr/testify
      #:tests? #f))
    (propagated-inputs
     (list go-github-com-dolthub-maphash))
    (home-page "https://github.com/dolthub/swiss")
    (synopsis "Swiss table hash map implementation")
    (description
     "This package provides a high-performance hash map implementation based
on Google's Swiss tables design.")
    (license license:asl2.0)))

;; TODO: Package go-github-com-dolthub-jsonpath
;; Requires: gopkg.in/src-d/go-errors.v1 (not yet packaged)
