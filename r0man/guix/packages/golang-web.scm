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

