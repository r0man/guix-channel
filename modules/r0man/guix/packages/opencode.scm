(define-module (r0man guix packages opencode)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary))

(define %opencode-version "2.1.4")

(define (opencode-binary arch hash)
  (origin
    (method url-fetch)
    (uri (string-append
          "https://opencode.ai/files/bin/" %opencode-version
          "/opencode-linux-" arch ".tar.gz"))
    (sha256 (base32 hash))))

(define-public opencode
  (package
    (name "opencode")
    (version %opencode-version)
    (source
     (let-system system
       (if (string-prefix? "aarch64" system)
           (opencode-binary
            "arm64" "0593i3fvbb5cbqq2w85nfmy2hf1apg1nrz1xr6y82sr4gdp9v73m")
           (opencode-binary
            "x64" "0vazbky4mfwr8lxdhwqnbaggap1l0dmik8wnx5gfq7g8jv7zinif"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan
      #~'(("opencode"))
      #:install-plan
      #~'(("opencode" "lib/opencode/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chmod-binary
            (lambda _
              (chmod "opencode" #o755)))
          (add-after 'install 'wrap-and-symlink-binary
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (gcc-lib (assoc-ref inputs "gcc:lib"))
                     (rg (search-input-file inputs "/bin/rg"))
                     (opencode (string-append out "/lib/opencode/opencode"))
                     (bin (string-append out "/bin")))
                (wrap-program opencode
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append gcc-lib "/lib")))
                  `("PATH" ":" prefix
                    (,(dirname rg))))
                (mkdir-p bin)
                (symlink opencode (string-append bin "/opencode"))
                (symlink opencode (string-append bin "/opencode2"))))))))
    (inputs (list (list "bash-minimal" bash-minimal)
                  (list "gcc:lib" gcc "lib")
                  (list "ripgrep" ripgrep)))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://opencode.ai")
    (synopsis "AI coding agent built for the terminal")
    (description
     "opencode is an open source AI coding agent for the terminal.  It
provides a full-screen terminal user interface as well as non-interactive
@command{run} and @command{serve} modes, and it can drive external editors
through the Agent Client Protocol.  The agent reads and edits files, runs
commands, and uses language servers for diagnostics, and it works with
models from many providers, including Anthropic, OpenAI, Google, and local
models served through an OpenAI-compatible API.  Version 2 adds background
subagents and shell commands, code mode for composing MCP servers and
OpenAPI endpoints from scripts, and a shared local service that the
terminal, web, and desktop clients all talk to.  This package installs the
standalone executable built by Bun from upstream release tarballs.")
    (license license:expat)))
