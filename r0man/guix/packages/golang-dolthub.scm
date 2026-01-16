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
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (r0man guix packages golang-xyz)
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

(define-public go-github-com-dolthub-jsonpath
  (let ((commit "19675ab05c71df43bda05c9f24e73942a5bb9483")
        (revision "0"))
    (package
      (name "go-github-com-dolthub-jsonpath")
      (version (git-version "0.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolthub/jsonpath")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "04xy8j80h4gpb6i2xci06j823aqjq7ym274l4v983v03zhykqdi7"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/dolthub/jsonpath"
        ;; Tests require testify
        #:tests? #f))
      (propagated-inputs
       (list go-github-com-pkg-errors
             go-gopkg-in-src-d-go-errors-v1))
      (home-page "https://github.com/dolthub/jsonpath")
      (synopsis "JSONPath implementation for Go")
      (description
       "This package implements JSONPath query language for extracting data
from JSON documents.")
      (license license:asl2.0))))

(define-public go-github-com-dolthub-sqllogictest-go
  (let ((commit "816f3ae12d8148a10fc7ca03bd48bdc47035d0ee")
        (revision "0"))
    (package
      (name "go-github-com-dolthub-sqllogictest-go")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolthub/sqllogictest")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "18n3qpziv6nn2jh1x9kfaz1sw46mhxk3s94f299n4dw8908vacwn"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/dolthub/sqllogictest/go/logictest"
        #:unpack-path "github.com/dolthub/sqllogictest"
        ;; Tests require database connections
        #:tests? #f))
      (propagated-inputs
       (list go-github-com-go-sql-driver-mysql
             go-github-com-stretchr-testify))
      (home-page "https://github.com/dolthub/sqllogictest")
      (synopsis "SQL Logic Test framework for Go")
      (description
       "This package provides a SQL Logic Test framework for validating SQL
database implementations against the official SQL Logic Test suite.")
      (license license:asl2.0))))

(define-public go-gopkg-in-src-d-go-errors-v1
  (package
    (name "go-gopkg-in-src-d-go-errors-v1")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/src-d/go-errors.v1")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fqsrjm6asqhvkk24plwyzf81scvg9fz0p74k3b272jwwi49gd7v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/src-d/go-errors.v1"
      #:tests? #f
      #:install-source? #t
      #:skip-build? #t))
    (propagated-inputs
     (list go-github-com-pkg-errors))
    (home-page "https://gopkg.in/src-d/go-errors.v1")
    (synopsis "Error handling library for Go")
    (description
     "This package provides utilities for creating and handling errors in Go
with support for error kinds and stack traces.")
    (license license:expat)))

(define-public go-github-com-dolthub-gozstd
  (package
    (name "go-github-com-dolthub-gozstd")
    (version "0.0.0-20240423170813-23a2903bca63")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/gozstd")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gi13nmg7cc2vbr1si0i073gid9kxsmvnppnww0yxmm1rkklp8lx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/gozstd"
      ;; Tests require test data files
      #:tests? #f))
    (native-inputs
     (list zstd))
    (home-page "https://github.com/dolthub/gozstd")
    (synopsis "CGO wrapper for Zstandard compression")
    (description
     "This package provides Go bindings to the Zstandard compression library
for high-performance compression used by Dolt.")
    (license license:asl2.0)))

(define-public go-github-com-dolthub-go-icu-regex
  (package
    (name "go-github-com-dolthub-go-icu-regex")
    (version "0.0.0-20230524105445-af7e7991c97e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/go-icu-regex")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05642ds8cm6ka5wjg14a1p0rh44shqnk2qwy6gmsdz1bcxan50al"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/go-icu-regex"
      ;; Tests require test files
      #:tests? #f))
    (propagated-inputs
     (list go-github-com-tetratelabs-wazero
           go-gopkg-in-src-d-go-errors-v1))
    (native-inputs
     (list icu4c))
    (home-page "https://github.com/dolthub/go-icu-regex")
    (synopsis "ICU regex bindings for Go")
    (description
     "This package provides minimal CGO bindings to ICU regex implementation
for MySQL-compatible regular expressions.")
    (license license:asl2.0)))

(define-public go-github-com-dolthub-flatbuffers-v23
  (package
    (name "go-github-com-dolthub-flatbuffers-v23")
    (version "23.3.3-dh.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/flatbuffers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g5f7kc2s8ff5kqdrzwk5a6sm99mziynaqscbyzqnjb56qprswac"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/flatbuffers/v23/go"
      #:unpack-path "github.com/dolthub/flatbuffers/v23"))
    (home-page "https://github.com/dolthub/flatbuffers")
    (synopsis "Dolthub fork of Google FlatBuffers for Go")
    (description
     "This package provides a memory efficient serialization library, a
dolthub fork of Google FlatBuffers with custom modifications for Dolt.")
    (license license:asl2.0)))

(define-public go-github-com-go-sql-driver-mysql-1.9
  (package
    (inherit go-github-com-go-sql-driver-mysql)
    (version "1.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-sql-driver/mysql")
             (commit (string-append "v" version))))
       (file-name (git-file-name "go-github-com-go-sql-driver-mysql" version))
       (sha256
        (base32 "01n2aj6rgb51cfcd9z68wpkd1ha9c8hh7mfaywq6sjxnn67qvc6z"))))))

(define-public go-github-com-jinzhu-inflection
  (package
    (name "go-github-com-jinzhu-inflection")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jinzhu/inflection")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "165i20d11s03771gi43skl66salxj36212r25fbs0cgr4qgfj7fy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jinzhu/inflection"))
    (home-page "https://github.com/jinzhu/inflection")
    (synopsis "Pluralization and singularization for Go")
    (description
     "This package provides utilities for inflecting English words between
singular and plural forms in Go applications.")
    (license license:expat)))

(define-public go-github-com-jinzhu-now
  (package
    (name "go-github-com-jinzhu-now")
    (version "1.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jinzhu/now")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10ywpaxs6d3y8gqlzx6rh3yw4ya83bnx0hrs0k0wq5bxbjhfmlil"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jinzhu/now"))
    (home-page "https://github.com/jinzhu/now")
    (synopsis "Time utilities for Go")
    (description
     "This package provides convenient time manipulation utilities for Go,
including functions for getting the beginning and end of various time periods.")
    (license license:expat)))

(define-public go-gorm-io-gorm
  (package
    (name "go-gorm-io-gorm")
    (version "1.25.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-gorm/gorm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1klhggxqxgksm5x1wja2r1nb6wphxyx15b7vjqrmlgznqb2skf68"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gorm.io/gorm"
      ;; Tests require database connections
      #:tests? #f))
    (propagated-inputs
     (list go-github-com-jinzhu-inflection
           go-github-com-jinzhu-now))
    (home-page "https://gorm.io")
    (synopsis "The fantastic ORM library for Go")
    (description
     "GORM is a developer-friendly ORM library for Go, providing associations,
hooks, transactions, and many other features for database operations.")
    (license license:expat)))

(define-public go-gorm-io-driver-mysql
  (package
    (name "go-gorm-io-driver-mysql")
    (version "1.5.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-gorm/mysql")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "161f69jrhip3wijjsxl5jfqhynj9i7y7v93h6zz71648a5ayl03q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gorm.io/driver/mysql"
      ;; Tests require database connections
      #:tests? #f))
    (propagated-inputs
     (list go-github-com-go-sql-driver-mysql-1.9
           go-gorm-io-gorm))
    (home-page "https://github.com/go-gorm/mysql")
    (synopsis "MySQL driver for GORM")
    (description
     "This package provides the official MySQL driver for the GORM ORM
framework, enabling database operations with MySQL databases.")
    (license license:expat)))

(define-public go-github-com-dolthub-vitess
  (let ((commit "205efc8530f1d6db26a342ec86f6daf8505ef83f")
        (revision "0"))
    (package
      (name "go-github-com-dolthub-vitess")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolthub/vitess")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "077cvp5dglcxn6wzvnl1anwspknjva8hxk65vyyd07lj9baachvg"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/dolthub/vitess/go"
        #:unpack-path "github.com/dolthub/vitess"
        #:tests? #f  ; Tests require additional setup
        #:install-source? #t
        #:skip-build? #t))  ; Source-only package with multiple modules
      (propagated-inputs
       (list go-github-com-google-go-cmp
             go-github-com-stretchr-testify
             go-golang-org-x-tools
             go-google-golang-org-grpc
             go-google-golang-org-protobuf))
      (home-page "https://github.com/dolthub/vitess")
      (synopsis "Vitess distributed database toolkit (dolthub fork)")
      (description
       "This package provides a database clustering system for horizontal
scaling of MySQL, forked and modified by dolthub for Dolt integration.")
      (license license:asl2.0))))

(define-public go-github-com-dolthub-go-mysql-server
  (let ((commit "d7eb602c04eef7f5966c87fd3c39c3a7b6f435e0")
        (revision "0"))
    (package
      (name "go-github-com-dolthub-go-mysql-server")
      (version (git-version "0.18.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolthub/go-mysql-server")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "19awcahhlfnqx9m7pqyjqbwadsq7waxpwiwdwg8jjajqn958mxky"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/dolthub/go-mysql-server"
        ;; Tests require database setup and extensive test data
        #:tests? #f
        #:install-source? #t
        #:skip-build? #t))  ; Source-only package, library code only
      (propagated-inputs
       (list go-github-com-cespare-xxhash-v2
             go-github-com-dolthub-flatbuffers-v23
             go-github-com-dolthub-go-icu-regex
             go-github-com-dolthub-jsonpath
             go-github-com-dolthub-sqllogictest-go
             go-github-com-dolthub-vitess
             go-github-com-go-sql-driver-mysql
             go-github-com-google-uuid
             go-github-com-hashicorp-golang-lru
             go-github-com-pkg-errors
             go-github-com-pmezard-go-difflib
             go-github-com-shopspring-decimal
             go-github-com-sirupsen-logrus
             go-github-com-stretchr-testify
             go-golang-org-x-exp
             go-golang-org-x-sync
             go-golang-org-x-sys
             go-golang-org-x-text
             go-golang-org-x-tools
             go-google-golang-org-grpc
             go-gopkg-in-src-d-go-errors-v1
             go-gopkg-in-yaml-v3))
      (home-page "https://github.com/dolthub/go-mysql-server")
      (synopsis "Pure Go MySQL-compatible database engine")
      (description
       "This package provides a complete SQL engine implementation in Go with
MySQL wire protocol compatibility.  It is used by Dolt for SQL query
processing and provides a full-featured MySQL-compatible SQL database that
can be embedded in Go applications.")
      (license license:asl2.0))))

(define-public go-github-com-dolthub-dolt-go-gen-proto-dolt-services-eventsapi
  (let ((commit "02e9f99a3a9be4822078a1435267ce8884756d29")
        (revision "0"))
    (package
      (name "go-github-com-dolthub-dolt-go-gen-proto-dolt-services-eventsapi")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolthub/dolt")
               (commit commit)))
         (file-name (git-file-name "go-github-com-dolthub-dolt" version))
         (sha256
          (base32 "0hmvdmx1j2fh7mcc965n2rxhkr4fnpxc5jyrgxd5h6a0digmqhn2"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/dolthub/dolt/go/gen/proto/dolt/services/eventsapi/v1alpha1"
        #:unpack-path "github.com/dolthub/dolt"
        #:tests? #f))  ; Tests not applicable for generated protobuf code
      (propagated-inputs
       (list go-google-golang-org-grpc
             go-google-golang-org-protobuf))
      (home-page "https://github.com/dolthub/dolt")
      (synopsis "Protocol buffer definitions for Dolt events API")
      (description
       "This package provides generated protobuf code for Dolt's events API,
used for telemetry and event tracking in Dolt applications.")
      (license license:asl2.0))))

(define-public go-github-com-dolthub-dolt-go
  (let ((commit "bcf4dd5f5cc1ca460a1712305d499b26f6036f1f")
        (revision "0"))
    (package
      (name "go-github-com-dolthub-dolt-go")
      (version (git-version "0.40.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolthub/dolt")
               (commit commit)))
         (file-name (git-file-name "go-github-com-dolthub-dolt" version))
         (sha256
          (base32 "14qybp3h7zjms03jcv07xlfqyshiz3fpina3gw3zmmwsn5rd9cbq"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Remove circular symlinks that cause unpacking issues
             (delete-file "go/gen/proto/github.com/dolthub/dolt/go/gen/proto")
             (delete-file "go/performance/utils/benchmark_runner/tpcc.lua")))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/dolthub/dolt/go"
        #:unpack-path "github.com/dolthub/dolt"
        ;; Tests require database setup and extensive test data
        #:tests? #f
        #:install-source? #t
        #:skip-build? #t))  ; Source-only package with multiple modules
      (propagated-inputs
       (list go-github-com-burntsushi-toml
             go-github-com-dolthub-flatbuffers-v23
             go-github-com-dolthub-fslock
             go-github-com-dolthub-go-mysql-server
             go-github-com-dolthub-gozstd
             go-github-com-dolthub-sqllogictest-go
             go-github-com-dolthub-swiss
             go-github-com-dolthub-vitess
             go-github-com-fatih-color
             go-github-com-go-sql-driver-mysql
             go-github-com-google-uuid
             go-github-com-pkg-errors
             go-github-com-shopspring-decimal
             go-github-com-sirupsen-logrus
             go-github-com-stretchr-testify
             go-golang-org-x-crypto
             go-golang-org-x-net
             go-golang-org-x-sync
             go-golang-org-x-sys
             go-google-golang-org-grpc
             go-google-golang-org-protobuf
             go-gopkg-in-src-d-go-errors-v1
             go-gopkg-in-yaml-v2))
      (home-page "https://github.com/dolthub/dolt")
      (synopsis "Dolt version-controlled database library")
      (description
       "This package provides the core libraries for Dolt, a version-controlled
SQL database.  It provides storage, versioning, and merge capabilities for
building applications with Git-like version control for database data.")
      (license license:asl2.0))))

(define-public go-github-com-dolthub-driver
  (package
    (name "go-github-com-dolthub-driver")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/driver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ikgxbfr02lxrygmzzzww81xz2r8dllrsc9q9rzz8z2y9c798h42"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/driver"
      ;; Tests require database setup and running Dolt server
      #:tests? #f
      #:install-source? #t
      #:skip-build? #t))  ; Source-only package, library code only
    (propagated-inputs
     (list go-github-com-dolthub-dolt-go
           go-github-com-dolthub-go-mysql-server
           go-github-com-dolthub-vitess
           go-github-com-go-sql-driver-mysql
           go-github-com-stretchr-testify
           go-gorm-io-driver-mysql
           go-gorm-io-gorm))
    (home-page "https://github.com/dolthub/driver")
    (synopsis "Database/sql driver for embedded Dolt")
    (description
     "This package provides a Go @code{database/sql} compatible driver for
embedding Dolt databases within Go applications.  It enables local access to
Dolt databases through the file system, similar to SQLite, without requiring
a separate server process.")
    (license license:asl2.0)))

(define-public go-github-com-juju-gnuflag
  (package
    (name "go-github-com-juju-gnuflag")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/juju/gnuflag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rky87fv1nbmfk12c1m478gvl3xi8jx6d353xay4zlj286mqdbhg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/juju/gnuflag"))
    (home-page "https://github.com/juju/gnuflag")
    (synopsis "GNU-compatible command-line flag parsing for Go")
    (description
     "This package provides a GNU-compatible command-line flag parsing library
for Go.  It supports GNU-style long options with single-dash short options,
making it suitable for command-line tools that follow GNU conventions.")
    (license license:lgpl3)))
