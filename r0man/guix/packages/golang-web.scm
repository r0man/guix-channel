(define-module (r0man guix packages golang-web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public go-github-com-aws-smithy-go
  (package
    (name "go-github-com-aws-smithy-go")
    (version "1.22.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/smithy-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jm5d8ampysimhnm3ginfv0fh8cmp4f2fghisk5lk0zsc0anahrm"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (for-each delete-file-recursively
                      (list "aws-http-auth"
                            "codegen"
                            "metrics/smithyotelmetrics"
                            "tracing/smithyoteltracing"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/smithy-go"))
    (home-page "https://github.com/aws/smithy-go")
    (synopsis "Smithy code generators for Go")
    (description
     "Package smithy provides the core components for a Smithy SDK.")
    (license license:asl2.0)))

(define-public go-github-com-aws-aws-sdk-go-v2
  (package
    (name "go-github-com-aws-aws-sdk-go-v2")
    (version "1.36.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go-v2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07g7vgpkq8cqirc2s64d9yswnpzdb7jzqr5kwrpblya2nq27inml"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (for-each delete-file-recursively
                      (list "config"
                            "service/s3"
                            "service/sqs"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/aws-sdk-go-v2"
      #:test-subdirs #~(list ".")))
    (propagated-inputs
     (list go-github-com-jmespath-go-jmespath
           go-github-com-aws-smithy-go))
    (home-page "https://github.com/aws/aws-sdk-go-v2")
    (synopsis "AWS SDK for Go v2")
    (description
     "Package sdk is the official AWS SDK for the Go programming language.")
    (license license:asl2.0)))

(define-public go-github-com-aws-aws-sdk-go-v2-config
  (package
    (name "go-github-com-aws-aws-sdk-go-v2-config")
    (version "1.29.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go-v2")
             (commit (go-version->git-ref version
                                          #:subdir "config"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07g7vgpkq8cqirc2s64d9yswnpzdb7jzqr5kwrpblya2nq27inml"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/aws-sdk-go-v2/config"
      #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
     (list go-github-com-aws-smithy-go))
    (home-page "https://github.com/aws/aws-sdk-go-v2")
    (synopsis "AWS SDK for Go v2 - config module")
    (description
     "Package config provides utilities for loading configuration from
multiple sources that can be used to configure the SDK's API clients, and
utilities.")
    (license license:asl2.0)))

(define-public go-github-com-aws-aws-sdk-go-v2-credentials
  (package
    (name "go-github-com-aws-aws-sdk-go-v2-credentials")
    (version "1.17.69")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go-v2")
             (commit (go-version->git-ref version
                                          #:subdir "credentials"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07g7vgpkq8cqirc2s64d9yswnpzdb7jzqr5kwrpblya2nq27inml"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/aws-sdk-go-v2/credentials"
      #:unpack-path "github.com/aws/aws-sdk-go-v2"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "test\\.go")
                  (("/bin/sleep") (which "sleep")))))))))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-aws-smithy-go))
    (home-page
     "https://pkg.go.dev/github.com/aws/aws-sdk-go-v2/credentials")
    (synopsis "AWS SDK for Go v2 - credentials module")
    (description
     "Package credentials provides types for retrieving credentials from
credentials sources.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric
  (package
    (name "go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric")
    (version "1.38.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "exporters/stdout/stdoutmetric"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h0di598nnj7223zh4ijs2blgp49mnww8rcm7iq0f2ic2l49czg5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "go.opentelemetry.io/otel/exporters/stdout/stdoutmetric"
      #:unpack-path "go.opentelemetry.io/otel"))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "STDOUT Metric Exporter")
    (description
     "Package stdoutmetric contains an @code{OpenTelemetry} exporter for metric
telemetry to be written to an output destination as JSON.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-otlp-otlplog-otlploghttp
  (package
    (name "go-go-opentelemetry-io-otel-exporters-otlp-otlplog-otlploghttp")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                      #:subdir "exporters/otlp/otlplog/otlploghttp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ag8103mf4j03lspi5rxk161iscbw1wkdk6ri9f746jia1w4qji8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path
      "go.opentelemetry.io/otel/exporters/otlp/otlplog/otlploghttp"
      #:unpack-path "go.opentelemetry.io/otel"))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OTLP HTTP Log Exporter")
    (description
     "Package otlploghttp provides an OTLP log exporter using HTTP with
protobuf payloads.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-log
  (package
    (name "go-go-opentelemetry-io-otel-log")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "log"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ag8103mf4j03lspi5rxk161iscbw1wkdk6ri9f746jia1w4qji8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "go.opentelemetry.io/otel/log"
      #:unpack-path "go.opentelemetry.io/otel"))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OpenTelemetry Logs API")
    (description
     "This package provides the OpenTelemetry Logs API for Go, intended to
be used by bridges between existing logging libraries and OpenTelemetry.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-sdk-log
  (package
    (name "go-go-opentelemetry-io-otel-sdk-log")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/log"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ag8103mf4j03lspi5rxk161iscbw1wkdk6ri9f746jia1w4qji8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "go.opentelemetry.io/otel/sdk/log"
      #:unpack-path "go.opentelemetry.io/otel"))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OpenTelemetry Log SDK")
    (description "Package log provides the OpenTelemetry Logs SDK for Go.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-otlp-otlpmetric-otlpmetrichttp
  (package
    (name
     "go-go-opentelemetry-io-otel-exporters-otlp-otlpmetric-otlpmetrichttp")
    (version "1.38.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                      #:subdir "exporters/otlp/otlpmetric/otlpmetrichttp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h0di598nnj7223zh4ijs2blgp49mnww8rcm7iq0f2ic2l49czg5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path
      "go.opentelemetry.io/otel/exporters/otlp/otlpmetric/otlpmetrichttp"
      #:unpack-path "go.opentelemetry.io/otel"))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OTLP HTTP Metric Exporter")
    (description
     "Package otlpmetrichttp provides an OTLP metrics exporter using HTTP with
protobuf payloads.")
    (license license:asl2.0)))

