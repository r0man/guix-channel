(define-module (r0man guix packages golang-dolthub)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages prometheus)
  #:use-module (gnu packages serialization)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (r0man guix packages golang-xyz))

;; Google Cloud Go packages for Dolt storage support

(define-public go-cloud-google-com-go-iam
  (package
    (name "go-cloud-google-com-go-iam")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version #:subdir "iam"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "022i18xrlav7fp6phdxshz6l3x3sdxb8klfmy8y1hx51sxnxwqab"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/iam"
      #:unpack-path "cloud.google.com/go"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://cloud.google.com/go/iam")
    (synopsis "Google Cloud IAM client library for Go")
    (description
     "This package provides a Go client library for Google Cloud Identity
and Access Management (IAM).  It enables managing permissions and access
control for Google Cloud resources.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go
  (package
    (name "go-cloud-google-com-go")
    (version "0.110.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yav01wrbyjb60y2yg60anxhbwl5y5y30jbwg8cd1s9hp49xb1rr"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove compute/metadata to avoid conflict with the standalone
        ;; go-cloud-google-com-go-compute-metadata package
        '(delete-file-recursively "compute/metadata"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs (list go-go-opencensus-io))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud client libraries for Go")
    (description
     "This package provides Go idiomatic client libraries for Google Cloud
Platform services.  It includes internal utilities and common types used by
other Google Cloud Go packages.")
    (license license:asl2.0)))

(define-public go-google-golang-org-genproto
  (package
    (name "go-google-golang-org-genproto")
    (version "0.0.0-20240624140628-dc46fd24d27d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-genproto")
             (commit "dc46fd24d27d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0az9lb8ma89v1lmp732cz8vp4w6p7vqs0a90cs4g1i0a5d2rdai6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "google.golang.org/genproto"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/googleapis/go-genproto")
    (synopsis "Generated Go packages for Google APIs")
    (description
     "This package contains the generated Go packages for common protocol
buffer types used by Google APIs, including googleapis/type packages for
date, expr, and other common types.")
    (license license:asl2.0)))

(define-public go-gopkg-in-src-d-go-errors-v1
  (package
    (name "go-gopkg-in-src-d-go-errors-v1")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/src-d/go-errors")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fqsrjm6asqhvkk24plwyzf81scvg9fz0p74k3b272jwwi49gd7v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/src-d/go-errors.v1"))
    (native-inputs (list go-github-com-stretchr-testify))
    (propagated-inputs (list go-github-com-pkg-errors))
    (home-page "https://github.com/src-d/go-errors")
    (synopsis "Error handling primitives with stack traces for Go")
    (description
     "This package provides error handling primitives with error wrapping and
error tracing for Go.  It enables creating typed errors using the Kind type
with built-in stack traces, formatting error messages with parameters, and
wrapping existing errors while maintaining error type information.")
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
      #:import-path "github.com/dolthub/maphash"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/dolthub/maphash")
    (synopsis "Hash any comparable type using Go's runtime hash")
    (description
     "This package allows hashing any comparable type using Go's fast runtime
hash.  It uses AES instructions when available for optimal performance.")
    (license license:asl2.0)))

(define-public go-github-com-dolthub-fslock
  (let ((commit "ef20baba23181a40c4ae76dfb303c99d3188e1c9")
        (revision "0"))
    (package
      (name "go-github-com-dolthub-fslock")
      (version (git-version "0.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolthub/fslock")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "126sk70ni646z9m4zzmvgl7g8vid1fg9sk4xyj53l9m5y95n4hay"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/dolthub/fslock"))
      (native-inputs (list go-gopkg-in-check-v1))
      (home-page "https://github.com/dolthub/fslock")
      (synopsis "Cross-platform file locking library for Go")
      (description
       "This package provides a cross-process mutex based on file locks that
works on Windows and Unix platforms.  It uses LockFileEx on Windows and flock
on Unix systems.")
      (license license:lgpl3))))

(define-public go-github-com-dolthub-jsonpath
  (package
    (name "go-github-com-dolthub-jsonpath")
    (version "0.0.2-0.20240227200619-19675ab05c71")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/jsonpath")
             (commit "19675ab05c71df43bda05c9f24e73942a5bb9483")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04xy8j80h4gpb6i2xci06j823aqjq7ym274l4v983v03zhykqdi7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/jsonpath"
      ;; Tests fail with Go 1.24+ due to non-constant format string
      #:tests? #f))
    (propagated-inputs (list go-gopkg-in-src-d-go-errors-v1))
    (home-page "https://github.com/dolthub/jsonpath")
    (synopsis "JSONPath implementation for Go")
    (description
     "This package provides a JSONPath implementation for Go, allowing queries
on JSON data structures using JSONPath expressions.")
    (license license:expat)))

(define-public go-github-com-dolthub-flatbuffers
  (package
    (name "go-github-com-dolthub-flatbuffers")
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
    (synopsis "Memory-efficient serialization library for Go")
    (description
     "FlatBuffers is a cross-platform serialization library for maximum memory
efficiency.  It allows direct access to serialized data without
parsing/unpacking while maintaining forwards/backwards compatibility.  This is
DoltHub's fork with Go-specific modifications.")
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
      #:import-path "github.com/dolthub/swiss"))
    (native-inputs (list go-github-com-stretchr-testify))
    (propagated-inputs (list go-github-com-dolthub-maphash))
    (home-page "https://github.com/dolthub/swiss")
    (synopsis "SwissTable hash map implementation for Go")
    (description
     "SwissMap is a hash table implementation adapted from Abseil's SwissTable
family.  It uses AES instructions for fast-hashing and performs key lookups
in parallel using SSE instructions.")
    (license license:asl2.0)))

(define-public go-github-com-dolthub-gozstd
  (package
    (name "go-github-com-dolthub-gozstd")
    (version "0.0.0-20240423170813-23a2903bca63")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/gozstd")
             (commit "23a2903bca636c86ad12f1e9daf42909e1687425")
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0313jc07dyk0lzxa97prjnhyb8qiwkal4v9ic5s0cjbya7jdnpwy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/gozstd"))
    (inputs (list zstd))
    (home-page "https://github.com/dolthub/gozstd")
    (synopsis "Go wrapper for Zstandard compression library")
    (description
     "This package provides a Go wrapper for the Zstandard (zstd) compression
library.  It builds zstd as part of the CGO build with optimization flags for
maximum performance.")
    (license license:expat)))

(define-public go-github-com-dolthub-go-icu-regex
  (package
    (name "go-github-com-dolthub-go-icu-regex")
    (version "0.0.0-20250916051405-78a38d478790")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/go-icu-regex")
             (commit "78a38d478790877d822b14380da7dc8d4fda2849")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bln24lbpv73nvdlh9qm0j6yrfphy52zf212vhmc62zfyim3kq8f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/go-icu-regex"
      ;; Library package; skip build (CGO requires ICU4C at compile time)
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs (list go-gopkg-in-src-d-go-errors-v1))
    (home-page "https://github.com/dolthub/go-icu-regex")
    (synopsis "ICU regex bindings for Go via CGO")
    (description
     "This package provides Go bindings to ICU4C's regex implementation via
CGO.  It supports MySQL-compatible regular expression functionality for use
in go-mysql-server and Dolt.")
    (license license:asl2.0)))

(define-public go-github-com-dolthub-vitess
  (package
    (name "go-github-com-dolthub-vitess")
    (version "0.0.0-20260128180459-bd171d35a7e2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/vitess")
             (commit "bd171d35a7e2b6c9aed0e943e082bc5cfc8de861")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fibza3y33bhv468fzvipmd4v755s6vsvhhhndabrw02wsvrj364"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/vitess/go"
      #:unpack-path "github.com/dolthub/vitess"
      ;; This is a library package with no main, just install source
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (native-inputs (list go-github-com-stretchr-testify))
    (propagated-inputs (list go-golang-org-x-tools
                             go-google-golang-org-grpc
                             go-google-golang-org-protobuf))
    (home-page "https://github.com/dolthub/vitess")
    (synopsis "MySQL-compatible SQL parser for Go")
    (description
     "This package provides DoltHub's fork of Vitess, a MySQL-compatible SQL
parser.  It extracts the SQL parsing components for use in go-mysql-server
and other projects requiring MySQL SQL parsing capabilities.")
    (license license:asl2.0)))

(define-public go-github-com-dolthub-go-mysql-server
  (package
    (name "go-github-com-dolthub-go-mysql-server")
    (version "0.20.1-0.20260128201837-5b7ec92cc6e9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/go-mysql-server")
             (commit "5b7ec92cc6e9a9f0d1798fce74178a240cdb28d0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p3fv77vqaswpfrq6wam7fd9vd8k3md3130irmjaxj7ma8n66lly"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/go-mysql-server"
      ;; This is a library package
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'unpack 'fix-embed-symlinks
            (lambda _
              (use-modules (ice-9 ftw))
              ;; Replace symlinked .bin files with actual copies
              (let ((encodings-dir (string-append
                                    "src/github.com/dolthub/go-mysql-server"
                                    "/sql/encodings")))
                (when (file-exists? encodings-dir)
                  (for-each (lambda (file)
                              (let ((path (string-append encodings-dir "/" file)))
                                (when (and (symbolic-link? path)
                                           (string-suffix? ".bin" file))
                                  (let ((target (readlink path)))
                                    (delete-file path)
                                    (copy-file target path)))))
                            (scandir encodings-dir
                                     (lambda (f)
                                       (not (member f '("." "..")))))))))))))
    (propagated-inputs
     (list go-github-com-cespare-xxhash
           go-github-com-dolthub-flatbuffers
           go-github-com-dolthub-go-icu-regex
           go-github-com-dolthub-jsonpath
           go-github-com-dolthub-vitess
           go-github-com-go-sql-driver-mysql
           go-github-com-gocraft-dbr-v2
           go-github-com-google-uuid
           go-github-com-hashicorp-golang-lru
           go-github-com-lestrrat-go-strftime
           go-github-com-pkg-errors
           go-github-com-shopspring-decimal
           go-github-com-sirupsen-logrus
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-text
           go-golang-org-x-tools
           go-google-golang-org-grpc
           go-gopkg-in-src-d-go-errors-v1
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/dolthub/go-mysql-server")
    (synopsis "MySQL-compatible SQL engine written in Go")
    (description
     "This package provides a MySQL-compatible SQL engine that can be used as a
library or embedded database.  It supports standard SQL queries and is
designed for use in Dolt, a version-controlled database.")
    (license license:asl2.0)))

(define-public go-github-com-dolthub-dolt-go-gen-proto-dolt-services-eventsapi
  (package
    (name "go-github-com-dolthub-dolt-go-gen-proto-dolt-services-eventsapi")
    (version "0.0.0-20240212175631-02e9f99a3a9b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/dolt")
             (commit "02e9f99a3a9b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hmvdmx1j2fh7mcc965n2rxhkr4fnpxc5jyrgxd5h6a0digmqhn2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/dolt/go/gen/proto/dolt/services/eventsapi"
      #:unpack-path "github.com/dolthub/dolt"
      ;; Only install the eventsapi proto package
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'check))))
    (propagated-inputs (list go-google-golang-org-grpc
                             go-google-golang-org-protobuf))
    (home-page "https://github.com/dolthub/dolt")
    (synopsis "Protocol buffer definitions for Dolt events API")
    (description
     "This package provides generated Protocol Buffer definitions for the Dolt
events API service, used for event tracking and telemetry in Dolt.")
    (license license:asl2.0)))

(define-public go-github-com-dolthub-eventsapi-schema
  (let ((commit "eadfd39051ca67f6555efbc79af066929fefd295")
        (revision "0"))
    (package
      (name "go-github-com-dolthub-eventsapi-schema")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolthub/eventsapi_schema")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1f3za7nyjw15sf0a9m77ysnbgy5vgmka3rfj06pisapd9pdv29b3"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/dolthub/eventsapi_schema"
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (delete 'build))))
      (propagated-inputs (list go-google-golang-org-grpc
                               go-google-golang-org-protobuf))
      (home-page "https://github.com/dolthub/eventsapi_schema")
      (synopsis "Protocol buffer schema for Dolt events API")
      (description
       "This package provides generated Protocol Buffer definitions for the Dolt
events API service, used for event tracking and telemetry in Dolt.")
      (license license:asl2.0))))

(define-public go-github-com-dolthub-aws-sdk-go-ini-parser
  (let ((commit "2821c37f6c12dd269368057faa32c9d3c5a21692")
        (revision "0"))
    (package
      (name "go-github-com-dolthub-aws-sdk-go-ini-parser")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolthub/aws-sdk-go-ini-parser")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1b1fpkb4s1z1d71r9bqkv3j0hr88mrk99hq0bp0syqidwg3k5l96"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/dolthub/aws-sdk-go-ini-parser"
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (delete 'build))))
      (home-page "https://github.com/dolthub/aws-sdk-go-ini-parser")
      (synopsis "INI file parser extracted from AWS SDK for Go")
      (description
       "This package provides an INI file parser extracted from the AWS SDK for
Go, used by Dolt for reading AWS configuration files.")
      (license license:asl2.0))))

(define-public go-github-com-gocraft-dbr-v2
  (package
    (name "go-github-com-gocraft-dbr-v2")
    (version "2.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gocraft/dbr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k63mk54q7r7b2ipcpdj6x7f6vr6p54iafnkifj6rppfsf4j2p4p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gocraft/dbr/v2"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/gocraft/dbr")
    (synopsis "Additions to Go's database/sql for SQL generation")
    (description
     "This package provides additions to Go's database/sql for super fast
performance and convenience, including a fluent SQL builder.")
    (license license:expat)))

(define-public go-github-com-esote-minmaxheap
  (package
    (name "go-github-com-esote-minmaxheap")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/esote/minmaxheap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mx1zqn7ll8rqmgrmwzwgi3d5ssvhg0cy767423sgbch7fj2nmhy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/esote/minmaxheap"))
    (home-page "https://github.com/esote/minmaxheap")
    (synopsis "Min-max heap for Go")
    (description
     "This package provides a min-max heap implementation for Go, enabling
logarithmic-time removal of both minimum and maximum elements.")
    (license license:cc0)))

(define-public go-github-com-dolthub-dolt-go
  (package
    (name "go-github-com-dolthub-dolt-go")
    (version "0.40.5-0.20260129204643-460dc5864c05")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/dolt")
             (commit "460dc5864c05a376b4b714609eb78192999ab43b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "031nbqcbccpbpzy505c6m924zw9w7b34nnsr1bhxqnqj4bvrl2x3"))
       (modules '((guix build utils)))
       (snippet
        ;; Replace google.golang.org/grpc/experimental/credentials
        ;; (not available in grpc v1.69.2) with standard credentials.NewTLS
        '(begin
           (substitute*
               "go/libraries/doltcore/env/grpc_dial_provider.go"
             (("\texpcreds \"google.golang.org/grpc/experimental/credentials\"\n")
              "")
             (("expcreds\\.NewTLSWithALPNDisabled")
              "credentials.NewTLS"))
           (substitute*
               "go/libraries/doltcore/sqle/cluster/controller.go"
             (("\texpcreds \"google.golang.org/grpc/experimental/credentials\"\n")
              "")
             (("expcreds\\.NewTLSWithALPNDisabled")
              "credentials.NewTLS"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/dolt/go"
      #:unpack-path "github.com/dolthub/dolt"
      ;; Library package with extensive dependencies
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs
     (list go-cloud-google-com-go
           go-cloud-google-com-go-iam
           go-cloud-google-com-go-storage
           go-github-com-aliyun-aliyun-oss-go-sdk
           go-github-com-aws-aws-sdk-go-v2
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
           go-github-com-aws-aws-sdk-go-v2-service-s3
           go-github-com-aws-aws-sdk-go-v2-service-sts
           go-github-com-aws-smithy-go
           go-github-com-bcicen-jstream
           go-github-com-cenkalti-backoff-v4
           go-github-com-denisbrodbeck-machineid
           go-github-com-dolthub-aws-sdk-go-ini-parser
           go-github-com-dolthub-eventsapi-schema
           go-github-com-dolthub-flatbuffers
           go-github-com-dolthub-fslock
           go-github-com-dolthub-go-mysql-server
           go-github-com-dolthub-gozstd
           go-github-com-dolthub-vitess
           go-github-com-dustin-go-humanize
           go-github-com-edsrzf-mmap-go
           go-github-com-esote-minmaxheap
           go-github-com-fatih-color
           go-github-com-go-sql-driver-mysql
           go-github-com-gocraft-dbr-v2
           go-github-com-goccy-go-json
           go-github-com-golang-snappy
           go-github-com-google-btree
           go-github-com-google-uuid
           go-github-com-googleapis-gax-go-v2
           go-github-com-hashicorp-golang-lru
           go-github-com-hashicorp-golang-lru-v2
           go-github-com-hdrhistogram-hdrhistogram-go
           go-github-com-juju-gnuflag
           go-github-com-kch42-buzhash
           go-github-com-kylelemons-godebug
           go-github-com-mohae-uvarint
           go-github-com-oracle-oci-go-sdk-v65
           go-github-com-pkg-errors
           go-github-com-pmezard-go-difflib
           go-github-com-sergi-go-diff
           go-github-com-shopspring-decimal
           go-github-com-silvasur-buzhash
           go-github-com-sirupsen-logrus
           go-github-com-spf13-cobra
           go-github-com-vbauerster-mpb-v8
           go-github-com-xitongsys-parquet-go
           go-github-com-xitongsys-parquet-go-source
           go-github-com-zeebo-xxh3
           go-go-uber-org-zap
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-oauth2
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-text
           go-google-golang-org-api
           go-google-golang-org-genproto
           go-google-golang-org-grpc
           go-google-golang-org-protobuf
           go-gopkg-in-go-jose-go-jose-v2
           go-gopkg-in-src-d-go-errors-v1
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/dolthub/dolt")
    (synopsis "Version-controlled SQL database engine")
    (description
     "Dolt is a SQL database with Git-like version control built in.  This
package provides the Go library for embedding Dolt functionality.")
    (license license:asl2.0)))

(define-public go-github-com-abiosoft-readline
  (package
    (name "go-github-com-abiosoft-readline")
    (version "0.0.0-20180607040430-155bce2042db")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abiosoft/readline")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "104q8dazj8yf6b089jjr82fy9h1g80zyyzvp3g8b44a7d8ngjj6r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/abiosoft/readline"
      #:tests? #f))
    (home-page "https://github.com/abiosoft/readline")
    (synopsis "Pure Go implementation of GNU Readline")
    (description
     "This package provides a pure Go implementation of a GNU Readline-like
library for building interactive command-line applications.")
    (license license:expat)))

(define-public go-github-com-andreyvit-diff
  (package
    (name "go-github-com-andreyvit-diff")
    (version "0.0.0-20170406064948-c7f18ee00883")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andreyvit/diff")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s4qjkxig5yqahpzfl4xqh4kzi9mymdpkzq6kj3f4dr5dl3hlynr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/andreyvit/diff"
      #:tests? #f))
    (propagated-inputs (list go-github-com-sergi-go-diff))
    (home-page "https://github.com/andreyvit/diff")
    (synopsis "Quick string diffing functions for Go")
    (description
     "This package provides string diffing functions based on
go-diff, mainly for use in tests.")
    (license license:expat)))

(define-public go-github-com-dolthub-ishell
  (package
    (name "go-github-com-dolthub-ishell")
    (version "0.0.0-20240701202509-2b217167d718")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/ishell")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h1j7rldaxlc7fi359il3p31v6ryfr8di51nd7yncnrfg4ra1vsd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/ishell"
      #:tests? #f))
    (propagated-inputs (list go-github-com-abiosoft-readline
                             go-github-com-fatih-color
                             go-github-com-flynn-archive-go-shlex))
    (home-page "https://github.com/dolthub/ishell")
    (synopsis "Interactive shell library for Go")
    (description
     "This package provides an interactive shell framework for building
command-line applications with auto-completion and command history.")
    (license license:expat)))

(define-public go-github-com-google-shlex
  (package
    (name "go-github-com-google-shlex")
    (version "0.0.0-20191202100458-e7afc7fbc510")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/shlex")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14z8hqyik910wk2qwnzgz8mjsmiamxa0pj55ahbv0jx6j3dgvzfm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/shlex"))
    (home-page "https://github.com/google/shlex")
    (synopsis "Shell-style string tokenizer for Go")
    (description
     "This package provides a simple lexer which splits input into tokens using
shell-style rules for quoting and commenting.")
    (license license:asl2.0)))

(define-public go-github-com-google-go-github-v57
  (package
    (name "go-github-com-google-go-github-v57")
    (version "57.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-github")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wqmqrmc0j17smxpby6fisaxgz0fy27h63kpz93nqmw5h04ia8m6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/go-github/v57"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs (list go-github-com-google-go-querystring))
    (home-page "https://github.com/google/go-github")
    (synopsis "Go client library for accessing the GitHub API")
    (description
     "This package provides a Go client library for accessing the GitHub API v3.")
    (license license:bsd-3)))

(define-public go-github-com-skratchdot-open-golang
  (package
    (name "go-github-com-skratchdot-open-golang")
    (version "0.0.0-20200116055534-eef842397966")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skratchdot/open-golang")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n6387csjn024db8wldadsiy8ljz7lk7szl6ls28fcbkax7rw86y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/skratchdot/open-golang"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/skratchdot/open-golang")
    (synopsis "Open URLs and files from Go using the OS default application")
    (description
     "This package provides a function to open URLs, files, and directories
using the default application associated by the operating system.")
    (license license:expat)))

(define-public go-github-com-tealeg-xlsx
  (package
    (name "go-github-com-tealeg-xlsx")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tealeg/xlsx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xkqk71sn10gaxl5r69ws52zi4ni3nfjkivpm9lsbk3xfqnzwv73"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tealeg/xlsx"
      #:tests? #f))
    (home-page "https://github.com/tealeg/xlsx")
    (synopsis "XLSX file reading and writing library for Go")
    (description
     "This package provides a library for reading and writing XLSX
(Microsoft Excel) spreadsheet files in Go.")
    (license license:bsd-3)))

(define-public go-github-com-tidwall-gjson
  (package
    (name "go-github-com-tidwall-gjson")
    (version "1.14.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tidwall/gjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rs8i87j62sgas55cvm8kzcgf40090dq38r6m6s03rzxs9hbcd6w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tidwall/gjson"
      #:tests? #f))
    (propagated-inputs (list go-github-com-tidwall-match
                             go-github-com-tidwall-pretty))
    (home-page "https://github.com/tidwall/gjson")
    (synopsis "Fast JSON value retrieval for Go")
    (description
     "This package provides a fast way to get values from JSON documents
using a simple path syntax.")
    (license license:expat)))

(define-public go-github-com-tidwall-sjson
  (package
    (name "go-github-com-tidwall-sjson")
    (version "1.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tidwall/sjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16yaikpxiwqz00zxa70w17k2k52nr06svand88sv2br6b6i8v09r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tidwall/sjson"))
    (propagated-inputs (list go-github-com-tidwall-gjson
                             go-github-com-tidwall-pretty))
    (home-page "https://github.com/tidwall/sjson")
    (synopsis "Fast JSON value setting for Go")
    (description
     "This package provides a fast way to set values in JSON documents
using a simple path syntax.")
    (license license:expat)))

(define-public go-github-com-shirou-gopsutil-v4/fixed
  (package
    (inherit go-github-com-shirou-gopsutil-v4)
    (arguments
     (list
      #:import-path "github.com/shirou/gopsutil/v4"
      #:tests? #f))))

(define-public go-github-com-yosida95-uritemplate-v3
  (package
    (name "go-github-com-yosida95-uritemplate-v3")
    (version "3.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yosida95/uritemplate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0csrdr64hjhwxlkcbb8y7bz1ccnyzl9c87fva00gr078nw52qxff"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yosida95/uritemplate/v3"
      #:tests? #f))
    (home-page "https://github.com/yosida95/uritemplate")
    (synopsis "URI template expansion library for Go")
    (description
     "This package provides URI template expansion following RFC 6570.")
    (license license:bsd-3)))

(define-public go-github-com-mark3labs-mcp-go
  (package
    (name "go-github-com-mark3labs-mcp-go")
    (version "0.34.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mark3labs/mcp-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0slx7bmpa72hp28fykw81ia32691bf9k60949xm680s1a16j257h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mark3labs/mcp-go"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs (list go-github-com-google-uuid
                             go-github-com-spf13-cast
                             go-github-com-yosida95-uritemplate-v3))
    (home-page "https://github.com/mark3labs/mcp-go")
    (synopsis "Model Context Protocol SDK for Go")
    (description
     "This package provides a Go implementation of the Model Context Protocol (MCP),
enabling AI assistants to interact with external tools and data sources.")
    (license license:expat)))

(define-public go-github-com-dolthub-dolt-mcp
  (package
    (name "go-github-com-dolthub-dolt-mcp")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/dolt-mcp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "014wvsjmqp8hk008mh467sabr8wvymlgd0izwjvrha2mjjkqw9z2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/dolt-mcp"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs (list go-github-com-mark3labs-mcp-go))
    (home-page "https://github.com/dolthub/dolt-mcp")
    (synopsis "Model Context Protocol server for Dolt databases")
    (description
     "This package provides a Model Context Protocol (MCP) server that enables
AI assistants to interact with Dolt version-controlled SQL databases.")
    (license license:asl2.0)))

(define-public dolt
  (package
    (inherit go-github-com-dolthub-dolt-go)
    (name "dolt")
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/dolthub/dolt/go/cmd/dolt"
      #:unpack-path "github.com/dolthub/dolt"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-home
            (lambda _ (setenv "HOME" "/tmp")))
          (add-after 'unpack 'fix-embed-symlinks
            (lambda _
              (use-modules (ice-9 ftw))
              (let ((encodings-dir (string-append
                                    "src/github.com/dolthub/go-mysql-server"
                                    "/sql/encodings")))
                (when (file-exists? encodings-dir)
                  (for-each (lambda (file)
                              (let ((path (string-append
                                           encodings-dir "/" file)))
                                (when (and (symbolic-link? path)
                                           (string-suffix? ".bin" file))
                                  (let ((target (readlink path)))
                                    (delete-file path)
                                    (copy-file target path)))))
                            (scandir encodings-dir
                                     (lambda (f)
                                       (not (member f '("." "..")))))))))))))
    (native-inputs
     (list ;; All propagated-inputs from go-github-com-dolthub-dolt-go
           go-cloud-google-com-go
           go-cloud-google-com-go-iam
           go-cloud-google-com-go-storage
           go-github-com-aliyun-aliyun-oss-go-sdk
           go-github-com-aws-aws-sdk-go-v2
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
           go-github-com-aws-aws-sdk-go-v2-service-s3
           go-github-com-aws-aws-sdk-go-v2-service-sts
           go-github-com-aws-smithy-go
           go-github-com-bcicen-jstream
           go-github-com-cenkalti-backoff-v4
           go-github-com-denisbrodbeck-machineid
           go-github-com-dolthub-aws-sdk-go-ini-parser
           go-github-com-dolthub-eventsapi-schema
           go-github-com-dolthub-flatbuffers
           go-github-com-dolthub-fslock
           go-github-com-dolthub-go-mysql-server
           go-github-com-dolthub-gozstd
           go-github-com-dolthub-vitess
           go-github-com-dustin-go-humanize
           go-github-com-edsrzf-mmap-go
           go-github-com-esote-minmaxheap
           go-github-com-fatih-color
           go-github-com-go-sql-driver-mysql
           go-github-com-gocraft-dbr-v2
           go-github-com-goccy-go-json
           go-github-com-golang-snappy
           go-github-com-google-btree
           go-github-com-google-uuid
           go-github-com-googleapis-gax-go-v2
           go-github-com-hashicorp-golang-lru
           go-github-com-hashicorp-golang-lru-v2
           go-github-com-hdrhistogram-hdrhistogram-go
           go-github-com-juju-gnuflag
           go-github-com-kch42-buzhash
           go-github-com-kylelemons-godebug
           go-github-com-mohae-uvarint
           go-github-com-oracle-oci-go-sdk-v65
           go-github-com-pkg-errors
           go-github-com-pmezard-go-difflib
           go-github-com-sergi-go-diff
           go-github-com-shopspring-decimal
           go-github-com-silvasur-buzhash
           go-github-com-sirupsen-logrus
           go-github-com-spf13-cobra
           go-github-com-vbauerster-mpb-v8
           go-github-com-xitongsys-parquet-go
           go-github-com-xitongsys-parquet-go-source
           go-github-com-zeebo-xxh3
           go-go-uber-org-zap
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-oauth2
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-text
           go-google-golang-org-api
           go-google-golang-org-genproto
           go-google-golang-org-grpc
           go-google-golang-org-protobuf
           go-gopkg-in-go-jose-go-jose-v2
           go-gopkg-in-src-d-go-errors-v1
           go-gopkg-in-yaml-v3
           ;; CLI-specific dependencies
           go-github-com-abiosoft-readline
           go-github-com-andreyvit-diff
           go-github-com-dolthub-dolt-mcp
           go-github-com-dolthub-ishell
           go-github-com-flynn-archive-go-shlex
           go-github-com-google-go-github-v57
           go-github-com-google-shlex
           go-github-com-pkg-profile
           go-github-com-prometheus-client-golang
           go-github-com-shirou-gopsutil-v4/fixed
           go-github-com-skratchdot-open-golang
           go-github-com-tealeg-xlsx
           go-github-com-tidwall-gjson
           go-github-com-tidwall-sjson
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-exporters-jaeger
           go-go-opentelemetry-io-otel-sdk
           go-go-opentelemetry-io-otel-trace
           go-gopkg-in-yaml-v2
           ;; CGO dependencies
           icu4c))
    (propagated-inputs '())
    (synopsis "SQL database with Git-like version control")
    (description
     "Dolt is a SQL database that supports Git-like version control features
such as clone, branch, merge, push, and pull for data and schema.  It provides
a CLI for managing versioned databases with full MySQL compatibility.")
    (license license:asl2.0)))

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
    (synopsis "English word pluralization library for Go")
    (description
     "This package provides utilities for pluralizing and singularizing
English words, commonly used in ORM frameworks.")
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
    (synopsis "Time toolkit for Go")
    (description
     "This package provides a time toolkit with utilities for parsing times,
calculating time ranges, and working with time boundaries.")
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
      ;; Tests require database
      #:tests? #f))
    (propagated-inputs (list go-github-com-jinzhu-inflection
                             go-github-com-jinzhu-now))
    (home-page "https://gorm.io")
    (synopsis "Object-Relational Mapping library for Go")
    (description
     "GORM is a full-featured ORM library for Go with support for
associations, hooks, preloading, transactions, and more.")
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
      ;; Tests require database
      #:tests? #f))
    (propagated-inputs (list go-github-com-go-sql-driver-mysql
                             go-gorm-io-gorm))
    (home-page "https://gorm.io")
    (synopsis "MySQL driver for GORM")
    (description
     "This package provides a MySQL database driver for the GORM ORM library.")
    (license license:expat)))

(define-public go-github-com-dolthub-driver
  (let ((commit "dc59f6dbac235b8b2d1b463922fc444d5b10a479")
        (revision "0"))
    (package
      (name "go-github-com-dolthub-driver")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolthub/driver")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0iyf3vj1ii5rr8x5kdzzzhv2aqvqak3s8zjw4w9dvsqkzpmzi14k"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/dolthub/driver"
        ;; Library package - install source only
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (delete 'build))))
      (propagated-inputs
       (list go-github-com-cenkalti-backoff-v4
             go-github-com-dolthub-dolt-go
             go-github-com-dolthub-go-mysql-server
             go-github-com-dolthub-vitess
             go-github-com-go-sql-driver-mysql
             go-gorm-io-driver-mysql
             go-gorm-io-gorm))
      (home-page "https://github.com/dolthub/driver")
      (synopsis "Go database/sql driver for embedded Dolt databases")
      (description
       "This package provides a Go database/sql compatible driver for Dolt,
enabling embedded version-controlled SQL databases in Go applications.")
      (license license:asl2.0))))

;; Additional dependencies for dolt

(define-public go-github-com-silvasur-buzhash
  (let ((commit "9bdec3dec7c611fa97beadc374d75bdf02cd880e")
        (revision "0"))
    (package
      (name "go-github-com-silvasur-buzhash")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/silvasur/buzhash")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "025z87cdmi958qhknpshlp9n4d071prrh4swasyxybnjqw5rgf1k"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/silvasur/buzhash"))
      (home-page "https://github.com/silvasur/buzhash")
      (synopsis "Rolling hash implementation for Go")
      (description
       "This package provides a rolling hash (buzhash) implementation for Go,
useful for content-defined chunking algorithms.")
      (license license:expat))))

(define-public go-github-com-kch42-buzhash
  (let ((commit "9bdec3dec7c611fa97beadc374d75bdf02cd880e")
        (revision "0"))
    (package
      (name "go-github-com-kch42-buzhash")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kch42/buzhash")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "025z87cdmi958qhknpshlp9n4d071prrh4swasyxybnjqw5rgf1k"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/kch42/buzhash"))
      (home-page "https://github.com/kch42/buzhash")
      (synopsis "Rolling hash implementation for Go")
      (description
       "This package provides a rolling hash (buzhash) implementation for Go.")
      (license license:expat))))

(define-public go-github-com-mohae-uvarint
  (let ((commit "c3f9e62bf2b0473a75b4e9cb7bea907f2cb307b4")
        (revision "0"))
    (package
      (name "go-github-com-mohae-uvarint")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mohae/uvarint")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0b16jb0fam328nmncx5y5vl7npvxmh84dj1m2bnsccprfxxfning"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/mohae/uvarint"))
      (home-page "https://github.com/mohae/uvarint")
      (synopsis "Unsigned variable-length integer encoding for Go")
      (description
       "This package provides unsigned variable-length integer encoding
utilities for Go.")
      (license license:expat))))

(define-public go-github-com-juju-gnuflag
  (package
    (name "go-github-com-juju-gnuflag")
    (version "0.0.0-20171113085948-2ce1bb71843d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/juju/gnuflag")
             (commit "2ce1bb71843d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rky87fv1nbmfk12c1m478gvl3xi8jx6d353xay4zlj286mqdbhg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/juju/gnuflag"))
    (home-page "https://github.com/juju/gnuflag")
    (synopsis "GNU-style command-line flag parsing for Go")
    (description
     "This package provides GNU-style command-line flag parsing for Go,
supporting long options with double dashes.")
    (license license:bsd-3)))

(define-public go-github-com-bcicen-jstream
  (package
    (name "go-github-com-bcicen-jstream")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bcicen/jstream")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hfnxcxnjgk0168jhp01fzarvhjly690ihp1bkx8hq84w83byvws"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bcicen/jstream"))
    (home-page "https://github.com/bcicen/jstream")
    (synopsis "Streaming JSON parser for Go")
    (description
     "This package provides a streaming JSON parser for Go that enables
processing large JSON documents without loading them entirely into memory.")
    (license license:expat)))

(define-public go-github-com-sergi-go-diff
  (package
    (name "go-github-com-sergi-go-diff")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sergi/go-diff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c7lsa3kjxbrx66r93d0pvx1408b80ignpi39fzka1qc0ylshw32"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sergi/go-diff/diffmatchpatch"
      #:unpack-path "github.com/sergi/go-diff"
      ;; Library package
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/sergi/go-diff")
    (synopsis "Diff, match and patch library for Go")
    (description
     "This package provides diff, match and patch algorithms for Go,
based on Google's diff-match-patch library.")
    (license license:expat)))

(define-public go-gopkg-in-errgo-v2
  (package
    (name "go-gopkg-in-errgo-v2")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-errgo/errgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "065mbihiy7q67wnql0bzl9y1kkvck5ivra68254zbih52jxwrgr2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/errgo.v2/errors"
      #:unpack-path "gopkg.in/errgo.v2"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/go-errgo/errgo")
    (synopsis "Error handling utilities for Go")
    (description
     "This package provides error handling utilities for Go with support
for error wrapping and location tracking.")
    (license license:bsd-3)))

(define-public go-gopkg-in-square-go-jose-v2
  (package
    (name "go-gopkg-in-square-go-jose-v2")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-jose/go-jose")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b1nhqxfmhzwrfk7pkvp2w3z3d0pf5ir00vizmy2d4xdbnldn70r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/square/go-jose.v2"
      #:unpack-path "gopkg.in/square/go-jose.v2"
      #:tests? #f))
    (propagated-inputs (list go-golang-org-x-crypto))
    (home-page "https://github.com/go-jose/go-jose")
    (synopsis "JOSE (JSON Object Signing and Encryption) library for Go")
    (description
     "This package provides a JOSE (JSON Object Signing and Encryption)
implementation for Go, supporting JWS, JWE, and JWT standards.")
    (license license:asl2.0)))

(define-public go-github-com-hashicorp-golang-lru-v2
  (package
    (name "go-github-com-hashicorp-golang-lru-v2")
    (version "2.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/golang-lru")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lb2ylv2bz6lsqhn6c2hsafjjcx0hsdbah6arhb778g3xbkpgvf3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/golang-lru/v2"))
    (home-page "https://github.com/hashicorp/golang-lru")
    (synopsis "LRU cache implementation for Go (v2)")
    (description
     "This package provides a thread-safe LRU cache implementation for Go
with support for various eviction policies.")
    (license license:mpl2.0)))

(define-public go-github-com-acarl005-stripansi
  (let ((commit "5a71ef0e047df0427e87a79f27009029921f1f9b")
        (revision "0"))
    (package
      (name "go-github-com-acarl005-stripansi")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/acarl005/stripansi")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02sxiishdixm791jqbkmhdcvc712l0fb8rqmibxzgc61h0qs6rs3"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/acarl005/stripansi"))
      (home-page "https://github.com/acarl005/stripansi")
      (synopsis "Strip ANSI escape codes from strings in Go")
      (description
       "This package provides a simple function to strip ANSI escape codes
from strings in Go.")
      (license license:expat))))

(define-public go-github-com-vbauerster-mpb-v8
  (package
    (name "go-github-com-vbauerster-mpb-v8")
    (version "8.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vbauerster/mpb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v6mj37hmw2cw8p6fzb1y90wj23f7fsl334yzydf1lx1n6n8jn8c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/vbauerster/mpb/v8"))
    (propagated-inputs (list go-github-com-acarl005-stripansi
                             go-github-com-mattn-go-runewidth
                             go-github-com-vividcortex-ewma
                             go-golang-org-x-sys))
    (home-page "https://github.com/vbauerster/mpb")
    (synopsis "Multi-progress bar for Go CLI applications")
    (description
     "This package provides a multi-progress bar library for Go terminal
applications, with support for spinners, decorators, and various bar styles.")
    (license license:unlicense)))

(define-public go-github-com-apache-thrift
  (package
    (name "go-github-com-apache-thrift")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/thrift")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x8m5n5nw80y4r81wl1w6yni6djm3wwfm7w1r6n16p8fdwafjprq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/apache/thrift/lib/go/thrift"
      #:unpack-path "github.com/apache/thrift"
      #:tests? #f))
    (home-page "https://github.com/apache/thrift")
    (synopsis "Apache Thrift Go library")
    (description
     "This package provides the Go library for Apache Thrift, an RPC framework
for scalable cross-language services development.")
    (license license:asl2.0)))

(define-public go-github-com-apache-arrow-go-arrow
  (let ((commit "651201b0f516")
        (revision "0"))
    (package
      (name "go-github-com-apache-arrow-go-arrow")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/apache/arrow")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ajwwy2976h8bdx1pdb826miyg4v8fss20c01wb6q48bgvqvh6an"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/apache/arrow/go/arrow"
        #:unpack-path "github.com/apache/arrow"
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (delete 'build))))
      (propagated-inputs (list go-github-com-google-flatbuffers
                               go-golang-org-x-xerrors))
      (home-page "https://arrow.apache.org/")
      (synopsis "Apache Arrow Go implementation")
      (description
       "This package provides the Go implementation of Apache Arrow, a
cross-language development platform for in-memory data.")
      (license license:asl2.0))))

(define-public go-github-com-xitongsys-parquet-go
  (package
    (name "go-github-com-xitongsys-parquet-go")
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xitongsys/parquet-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lx8r5jz18m7g9n07aqwbwn3m3y44h3g8ijams7cz5m2zp32sqr5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xitongsys/parquet-go"
      ;; Tests require parquet-go-source
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs (list go-github-com-apache-arrow-go-arrow
                             go-github-com-apache-thrift
                             go-github-com-golang-snappy
                             go-github-com-klauspost-compress
                             go-github-com-pierrec-lz4-v4))
    (home-page "https://github.com/xitongsys/parquet-go")
    (synopsis "Native Go implementation of Apache Parquet")
    (description
     "This package provides a native Go implementation of Apache Parquet
format for reading and writing Parquet files.")
    (license license:asl2.0)))

(define-public go-github-com-xitongsys-parquet-go-source
  (let ((commit "b732d2ac9c9b72cef06d154fcbfe7dafa0ffd21c")
        (revision "0"))
    (package
      (name "go-github-com-xitongsys-parquet-go-source")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/xitongsys/parquet-go-source")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12f0gabhalxaz19fb3pkap6y6b592h6cwf3amz0bcy39ifc3w9il"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/xitongsys/parquet-go-source"
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (delete 'build))))
      (propagated-inputs (list go-github-com-xitongsys-parquet-go))
      (home-page "https://github.com/xitongsys/parquet-go-source")
      (synopsis "Data source adapters for parquet-go")
      (description
       "This package provides data source adapters for parquet-go, enabling
reading and writing Parquet files from various sources including local
filesystem, HDFS, S3, and GCS.")
      (license license:asl2.0))))

(define-public go-github-com-go-kit-kit
  (package
    (name "go-github-com-go-kit-kit")
    (version "0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-kit/kit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c9vra93whqd0mf9g8krw8drlv9cv8v0b9abslvyms4qyy56j171"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-kit/kit"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs (list go-github-com-go-kit-log))
    (home-page "https://github.com/go-kit/kit")
    (synopsis "Microservices toolkit for Go")
    (description
     "Go kit is a collection of Go (golang) packages for building
microservices, including logging, metrics, tracing, and transport layers.")
    (license license:expat)))

(define-public go-cloud-google-com-go-storage
  (package
    (name "go-cloud-google-com-go-storage")
    (version "1.31.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (string-append "storage/v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w0bml1pprb86hf86h17ngq37pi6lwnplfc04z4mggi0kqxvb6p4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/storage"
      #:unpack-path "cloud.google.com/go"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    ;; Note: propagated-inputs removed to avoid file merging conflicts
    ;; in go-build-system's unpack phase.  Packages depending on this
    ;; should add the required dependencies directly.
    (home-page "https://cloud.google.com/go/storage")
    (synopsis "Google Cloud Storage client library for Go")
    (description
     "This package provides the Go client library for Google Cloud Storage.")
    (license license:asl2.0)))
