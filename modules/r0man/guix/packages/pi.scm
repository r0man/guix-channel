(define-module (r0man guix packages pi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nonguix build-system binary))

(define-public pi-coding-agent
  (package
    (name "pi-coding-agent")
    (version "0.70.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/badlogic/pi-mono/releases/download/v"
             version "/pi-linux-"
             (cond ((target-aarch64?) "arm64")
                   ((target-x86-64?) "x64")
                   (else "x64"))
             ".tar.gz"))
       (sha256
        (base32
         (cond ((target-aarch64?)
                "1vz6j1dwspc6n0a21v4syx2x76gnmhqhf6n2lx9mcwxza1805xci")
               ((target-x86-64?)
                "0jgp8rsfq02fblrdq9ypdj3x093lifn7cm7dmy6jddw6wil24cal")
               (else
                "0jgp8rsfq02fblrdq9ypdj3x093lifn7cm7dmy6jddw6wil24cal"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan #~'(("pi"))
      #:install-plan #~'(("." "lib/pi-coding-agent/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chmod-binary
            (lambda _
              (chmod "pi" #o755)))
          (add-after 'install 'symlink-binary
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (symlink (string-append #$output "/lib/pi-coding-agent/pi")
                         (string-append bin "/pi"))))))))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/badlogic/pi-mono")
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
