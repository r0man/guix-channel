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
  #:use-module (guix utils)
  #:use-module (r0man guix packages golang-xyz))

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
