(define-module (r0man guix packages pi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu packages gcc)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary))

(define %pi-coding-agent-version "0.85.1")

(define (pi-coding-agent-binary arch hash)
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/earendil-works/pi/releases/download/v"
          %pi-coding-agent-version "/pi-linux-" arch ".tar.gz"))
    (sha256 (base32 hash))))

(define-public pi-coding-agent
  (package
    (name "pi-coding-agent")
    (version %pi-coding-agent-version)
    (source
     (let-system system
       (if (string-prefix? "aarch64" system)
           (pi-coding-agent-binary
            "arm64" "1m74x8qcb34h6x0dwi7vx6r7ghv2p6034pw10aqz7r2yi2p20b84")
           (pi-coding-agent-binary
            "x64" "0np23zma1b5xbfswlsd166ax98r1x6jzd1ik1gs22kfp8y7ljkj9"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan
      #~'(("pi"))
      #:install-plan
      #~'(("." "lib/pi-coding-agent/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chmod-binary
            (lambda _
              (chmod "pi" #o755)))
          (add-after 'install 'wrap-and-symlink-binary
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; pi is a Bun standalone binary.  When it tears down MCP
              ;; stdio worker threads, glibc's pthread_exit lazily
              ;; dlopen()s libgcc_s.so.1 for stack unwinding, which only
              ;; resolves via LD_LIBRARY_PATH (not DT_NEEDED/RUNPATH).
              ;; Patchelfing the binary corrupts Bun's single-file ELF
              ;; layout, so wrap it instead.
              (let* ((out (assoc-ref outputs "out"))
                     (gcc-lib (assoc-ref inputs "gcc"))
                     (pi (string-append out "/lib/pi-coding-agent/pi"))
                     (bin (string-append out "/bin")))
                (wrap-program pi
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append gcc-lib "/lib"))))
                (mkdir-p bin)
                (symlink pi (string-append bin "/pi"))))))))
    (inputs (list bash (list gcc "lib")))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/earendil-works/pi")
    (synopsis "Minimal terminal coding agent")
    (description
     "Pi is a minimal terminal coding harness that adapts to your workflow
rather than forcing you into predefined patterns.  It provides an AI-powered
coding assistant with several operating modes: an interactive mode with a
text editor interface, a print/JSON mode for scripting, an RPC mode for
process integration via stdin/stdout, and an SDK mode for embedding in
custom applications.  The agent ships with basic tools for reading, writing,
and editing files as well as executing bash commands, and can be extended
through TypeScript extensions, reusable prompt templates, skills following
the Agent Skills standard, and customizable themes.  This package installs
the standalone executable built by Bun from upstream release tarballs.")
    (license license:expat)))
