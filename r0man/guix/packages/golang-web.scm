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

;; Override core OpenTelemetry packages from (gnu packages golang-web) to
;; version 1.42.0, needed by beads-next and gastown-next.  All subpackages
;; come from the same opentelemetry-go monorepo at the same commit.

(define otel-source/1.42
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/open-telemetry/opentelemetry-go")
          ;; Any v1.42.0 subpackage tag resolves to the same commit.
          (commit "sdk/metric/v1.42.0")))
    (file-name "opentelemetry-go-1.42.0-checkout")
    (sha256
     (base32 "1mxj3gv04lh0lss2fk4c5n23m55qm4zy1caa3fqxmnjhy0yjm6rf"))))

(define-public go-go-opentelemetry-io-otel
  (package
    (name "go-go-opentelemetry-io-otel")
    (version "1.42.0")
    (source otel-source/1.42)
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "go.opentelemetry.io/otel"
      #:unpack-path "go.opentelemetry.io/otel"))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OpenTelemetry API for Go")
    (description
     "This package provides the OpenTelemetry API for Go, including context
propagation, tracing, and metrics interfaces.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-trace
  (package
    (name "go-go-opentelemetry-io-otel-trace")
    (version "1.42.0")
    (source otel-source/1.42)
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "go.opentelemetry.io/otel/trace"
      #:unpack-path "go.opentelemetry.io/otel"))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OpenTelemetry Trace API for Go")
    (description
     "This package provides the OpenTelemetry Trace API for Go.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-metric
  (package
    (name "go-go-opentelemetry-io-otel-metric")
    (version "1.42.0")
    (source otel-source/1.42)
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "go.opentelemetry.io/otel/metric"
      #:unpack-path "go.opentelemetry.io/otel"))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OpenTelemetry Metrics API for Go")
    (description
     "This package provides the OpenTelemetry Metrics API for Go.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-sdk
  (package
    (name "go-go-opentelemetry-io-otel-sdk")
    (version "1.42.0")
    (source otel-source/1.42)
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "go.opentelemetry.io/otel/sdk"
      #:unpack-path "go.opentelemetry.io/otel"))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OpenTelemetry SDK for Go")
    (description
     "This package provides the OpenTelemetry SDK for Go.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-sdk-metric
  (package
    (name "go-go-opentelemetry-io-otel-sdk-metric")
    (version "1.42.0")
    (source otel-source/1.42)
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "go.opentelemetry.io/otel/sdk/metric"
      #:unpack-path "go.opentelemetry.io/otel"))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OpenTelemetry Metrics SDK for Go")
    (description
     "This package provides the OpenTelemetry Metrics SDK for Go.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-stdout-stdouttrace
  (package
    (name "go-go-opentelemetry-io-otel-exporters-stdout-stdouttrace")
    (version "1.42.0")
    (source otel-source/1.42)
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "go.opentelemetry.io/otel/exporters/stdout/stdouttrace"
      #:unpack-path "go.opentelemetry.io/otel"))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "STDOUT Trace Exporter")
    (description
     "Package stdouttrace contains an OpenTelemetry exporter for tracing
telemetry to be written to an output destination as JSON.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-proto-otlp
  (package
    (name "go-go-opentelemetry-io-proto-otlp")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-proto-go")
             (commit (string-append "otlp/v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hv5sqsp6r539nwbhyapwnqcpx1wipxlsgpp2w9di6zva0irvjb0"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove gRPC gateway generated files that pull in
        ;; grpc-ecosystem/grpc-gateway/v2 as a build dependency.
        ;; Only the core protobuf types are needed by consumers.
        '(begin
           (for-each delete-file
                     (find-files "." "\\.pb\\.gw\\.go$"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "go.opentelemetry.io/proto/otlp"
      #:unpack-path "go.opentelemetry.io/proto"))
    (home-page "https://go.opentelemetry.io/proto/otlp")
    (synopsis "OpenTelemetry Protocol Go bindings")
    (description
     "This package provides Go bindings for the OpenTelemetry Protocol (OTLP).")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric
  (package
    (name "go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric")
    (version "1.42.0")
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
        (base32 "1mxj3gv04lh0lss2fk4c5n23m55qm4zy1caa3fqxmnjhy0yjm6rf"))))
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
    (version "0.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                      #:subdir "exporters/otlp/otlplog/otlploghttp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mxj3gv04lh0lss2fk4c5n23m55qm4zy1caa3fqxmnjhy0yjm6rf"))))
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
    (version "0.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "log"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mxj3gv04lh0lss2fk4c5n23m55qm4zy1caa3fqxmnjhy0yjm6rf"))))
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
    (version "0.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/log"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mxj3gv04lh0lss2fk4c5n23m55qm4zy1caa3fqxmnjhy0yjm6rf"))))
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
    (version "1.42.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                      #:subdir "exporters/otlp/otlpmetric/otlpmetrichttp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mxj3gv04lh0lss2fk4c5n23m55qm4zy1caa3fqxmnjhy0yjm6rf"))))
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

(define-public go-github-com-anthropics-anthropic-sdk-go
  (let ((base (@ (gnu packages golang-web)
                 go-github-com-anthropics-anthropic-sdk-go)))
    (package
      (inherit base)
      (version "1.27.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/anthropics/anthropic-sdk-go")
               (commit (string-append "v" version))))
         (file-name (git-file-name "go-github-com-anthropics-anthropic-sdk-go"
                     version))
         (sha256
          (base32 "1018kspphasckf32xjl8a8rr6gzxzh1xdllkb81m4ybi30kf026d"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:tests? _ #t)
          #f)))
      (propagated-inputs (modify-inputs (package-propagated-inputs base)
                           (append go-golang-org-x-sync))))))

(define-public go-github-com-johanneskaufmann-dom
  (package
    (name "go-github-com-johanneskaufmann-dom")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JohannesKaufmann/dom")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00dmh5hz6cdjcm09j8qql85sfgmw315wzh2bjhgq06bkrb94dcmy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/JohannesKaufmann/dom"))
    (propagated-inputs (list go-golang-org-x-net))
    (home-page "https://github.com/JohannesKaufmann/dom")
    (synopsis "DOM utility functions for Go")
    (description "Package dom provides utility functions for working with
@code{golang.org/x/net/html} nodes.")
    (license license:expat)))

(define-public go-github-com-johanneskaufmann-html-to-markdown-v2
  (package
    (name "go-github-com-johanneskaufmann-html-to-markdown-v2")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JohannesKaufmann/html-to-markdown")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1047pgjadyf4fb4qk5wn9zfn088kibfrfr26l0wm83pd50vcjdy5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "github.com/JohannesKaufmann/html-to-markdown/v2"))
    (propagated-inputs (list go-github-com-johanneskaufmann-dom
                             go-github-com-andybalholm-cascadia
                             go-github-com-yuin-goldmark go-golang-org-x-net))
    (home-page "https://github.com/JohannesKaufmann/html-to-markdown")
    (synopsis "Convert HTML to Markdown in Go")
    (description
     "Package htmltomarkdown converts HTML to Markdown.  It supports
customization through rules and plugins.")
    (license license:expat)))

(define-public go-github-com-invopop-jsonschema
  (package
    (name "go-github-com-invopop-jsonschema")
    (version "0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/invopop/jsonschema")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0my5j2fycl0xf3vn02xzy6fr7dkf8nkn62f8y5i2xish69007vhm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/invopop/jsonschema"))
    (propagated-inputs (list go-github-com-wk8-go-ordered-map-v2
                             go-github-com-bahlo-generic-list-go
                             go-github-com-buger-jsonparser
                             go-github-com-mailru-easyjson))
    (home-page "https://github.com/invopop/jsonschema")
    (synopsis "Generate JSON Schemas from Go types")
    (description
     "This package generates JSON Schema documents from Go types using
reflection.  It supports struct tags for customizing the output schema.")
    (license license:expat)))

