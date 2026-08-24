(define-module (r0man guix packages claude)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary))

;; Starting with 2.1.119, upstream's npm distribution became a thin wrapper
;; that resolves platform-specific precompiled native binaries via
;; optionalDependencies — incompatible with Guix's node-build-system.  This
;; package fetches the per-platform prebuilt binary from Anthropic's
;; GitHub release (the canonical, signed upstream source) and patchelfs
;; the native ELF to use Guix's glibc.

(define %claude-code-version "2.1.227")

(define (claude-code-binary arch hash)
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/anthropics/claude-code/releases/download/v"
          %claude-code-version "/claude-linux-" arch ".tar.gz"))
    (sha256 (base32 hash))))

(define-public claude-code
  (package
    (name "claude-code")
    (version %claude-code-version)
    (source
     (let-system system
       (if (string-prefix? "aarch64" system)
           (claude-code-binary
            "arm64" "1cv8874pnsspjpdnwchqpjwjhsg225a0p7qkcszc5ir170bdz679")
           (claude-code-binary
            "x64" "0mgf24wdmd4wrlvs82gnqqak424398n1pb239mcpifal3j6n94gf"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan
      #~'(("claude"))
      #:install-plan
      #~'(("." "lib/claude-code/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chmod-binary
            (lambda _
              (chmod "claude" #o755)))
          (add-after 'install 'symlink-binary
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (symlink (string-append #$output "/lib/claude-code/claude")
                         (string-append bin "/claude"))))))))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/anthropics/claude-code")
    (synopsis "AI coding assistant that lives in your terminal")
    (description
     "Claude Code is an agentic coding tool that lives in your terminal.
It understands your codebase, edits files, runs terminal commands, and
handles entire workflows through natural language commands.  Powered by
Anthropic's Claude AI assistant.")
    (license (license:non-copyleft
              "https://github.com/anthropics/claude-code/blob/main/LICENSE.md"
              "See LICENSE.md in the repository."))))
