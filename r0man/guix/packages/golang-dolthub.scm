(define-module (r0man guix packages golang-dolthub)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
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
      #:import-path "github.com/dolthub/fslock"))
    (native-inputs (list go-gopkg-in-check-v1))
    (home-page "https://github.com/dolthub/fslock")
    (synopsis "Cross-platform file locking library for Go")
    (description
     "This package provides a cross-process mutex based on file locks that
works on Windows and Unix platforms.  It uses LockFileEx on Windows and flock
on Unix systems.")
    (license license:lgpl3)))

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
    (version "0.0.0-20230524105445-af7e7991c97e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/go-icu-regex")
             (commit "af7e7991c97e3c6ac3d94c3a1e31cc1b7c3729ec")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05642ds8cm6ka5wjg14a1p0rh44shqnk2qwy6gmsdz1bcxan50al"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dolthub/go-icu-regex"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-embed-symlinks
            (lambda _
              ;; Replace symlinked icu.wasm with actual copy
              (let ((wasm-file (string-append
                                "src/github.com/dolthub/go-icu-regex"
                                "/icu/wasm/icu.wasm")))
                (when (and (file-exists? wasm-file)
                           (symbolic-link? wasm-file))
                  (let ((target (readlink wasm-file)))
                    (delete-file wasm-file)
                    (copy-file target wasm-file)))))))))
    (native-inputs (list go-github-com-stretchr-testify))
    (propagated-inputs (list go-github-com-tetratelabs-wazero
                             go-gopkg-in-src-d-go-errors-v1))
    (home-page "https://github.com/dolthub/go-icu-regex")
    (synopsis "ICU regex bindings for Go via WebAssembly")
    (description
     "This package provides minimal bindings to ICU4C's regex implementation
via WebAssembly.  It supports MySQL-compatible regular expression functionality
using the wazero WebAssembly runtime.")
    (license license:asl2.0)))

(define-public go-github-com-dolthub-vitess
  (package
    (name "go-github-com-dolthub-vitess")
    (version "0.0.0-20240709194214-9938efd011aa")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/vitess")
             (commit "9938efd011aa2364b713cd3e647019156836c776")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wna0p25lvh6mj6xcxciclsb4j2dc44b014xij96h48g1ca70lsa"))))
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
    (version "0.18.2-0.20240702022058-d7eb602c04ee")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/go-mysql-server")
             (commit "d7eb602c04eef7f5966c87fd3c39c3a7b6f435e0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19awcahhlfnqx9m7pqyjqbwadsq7waxpwiwdwg8jjajqn958mxky"))))
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
           go-github-com-go-kit-kit
           go-github-com-dolthub-flatbuffers
           go-github-com-dolthub-go-icu-regex
           go-github-com-dolthub-jsonpath
           go-github-com-dolthub-vitess
           go-github-com-go-sql-driver-mysql
           go-github-com-google-uuid
           go-github-com-hashicorp-golang-lru
           go-github-com-lestrrat-go-strftime
           go-github-com-pkg-errors
           go-github-com-shopspring-decimal
           go-github-com-sirupsen-logrus
           go-golang-org-x-exp
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

(define-public go-github-com-dolthub-dolt-go
  (package
    (name "go-github-com-dolthub-dolt-go")
    (version "0.40.5-0.20240702155756-bcf4dd5f5cc1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolthub/dolt")
             (commit "bcf4dd5f5cc1ca460a1712305d499b26f6036f1f")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14qybp3h7zjms03jcv07xlfqyshiz3fpina3gw3zmmwsn5rd9cbq"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove eventsapi symlinks which cause issues during unpack
        '(delete-file-recursively "go/gen/proto/dolt/services/eventsapi"))))
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
           go-github-com-aws-aws-sdk-go
           go-github-com-bcicen-jstream
           go-github-com-boltdb-bolt
           go-github-com-cenkalti-backoff-v4
           go-github-com-denisbrodbeck-machineid
           go-github-com-dolthub-dolt-go-gen-proto-dolt-services-eventsapi
           go-github-com-dolthub-flatbuffers
           go-github-com-dolthub-fslock
           go-github-com-dolthub-go-mysql-server
           go-github-com-dolthub-gozstd
           go-github-com-dolthub-swiss
           go-github-com-dolthub-vitess
           go-github-com-dustin-go-humanize
           go-github-com-fatih-color
           go-github-com-go-sql-driver-mysql
           go-github-com-go-kit-kit
           go-github-com-goccy-go-json
           go-github-com-golang-snappy
           go-github-com-google-btree
           go-github-com-google-uuid
           go-github-com-googleapis-gax-go-v2
           go-github-com-hashicorp-golang-lru
           go-github-com-hashicorp-golang-lru-v2
           go-github-com-hdrhistogram-hdrhistogram-go
           go-github-com-jpillora-backoff
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
           go-gopkg-in-errgo-v2
           go-gopkg-in-square-go-jose-v2
           go-gopkg-in-src-d-go-errors-v1
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/dolthub/dolt")
    (synopsis "Version-controlled SQL database engine")
    (description
     "Dolt is a SQL database with Git-like version control built in.  This
package provides the Go library for embedding Dolt functionality.")
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
      ;; Library package - install source only
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs
     (list go-github-com-dolthub-dolt-go
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
    (license license:asl2.0)))

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
