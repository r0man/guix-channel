(define-module (r0man guix packages github-actions)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages tls)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary))

(define %actions-runner-version "2.337.0")

(define (actions-runner-binary arch hash)
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/actions/runner/releases/download/v"
          %actions-runner-version "/actions-runner-linux-" arch "-"
          %actions-runner-version ".tar.gz"))
    (sha256 (base32 hash))))

(define-public actions-runner
  (package
    (name "actions-runner")
    (version %actions-runner-version)
    (source
     (let-system system
       (if (string-prefix? "aarch64" system)
           (actions-runner-binary
            "arm64" "14q37v0ylbxz3543yaillmfv3gln901cyrs7r7ijc9a24q3cf7cv")
           (actions-runner-binary
            "x64" "04v6h5ih0787d2448lmb5ya1r726qsjvr0l6h4l47bgqlh8hi4kh"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan
      #~'(;; .NET apphost executables + tools
          ("bin/Runner.Listener" ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/Runner.Worker" ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/Runner.PluginHost" ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/createdump" ("gcc" "zlib" "openssl" "icu4c"))
          ;; Bundled Node.js executables (for running JavaScript actions)
          ("externals/node20/bin/node" ("gcc" "zlib" "openssl" "icu4c"))
          ("externals/node24/bin/node" ("gcc" "zlib" "openssl" "icu4c"))
          ;; .NET native shared libraries (rpath-only, no interpreter)
          ("bin/libclrgc.so" ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libclrjit.so" ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libcoreclr.so" ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libcoreclrtraceptprovider.so"
           ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libhostfxr.so" ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libhostpolicy.so" ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libmscordaccore.so" ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libmscordbi.so" ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libSystem.Globalization.Native.so"
           ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libSystem.IO.Compression.Native.so"
           ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libSystem.Native.so" ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libSystem.Net.Security.Native.so"
           ("gcc" "zlib" "openssl" "icu4c"))
          ("bin/libSystem.Security.Cryptography.Native.OpenSsl.so"
           ("gcc" "zlib" "openssl" "icu4c")))
      #:install-plan
      #~'(("." "lib/actions-runner/"
           #:exclude ("environment-variables")))
      #:phases
      #~(modify-phases %standard-phases
          ;; The gnu-build-system 'unpack' phase does
          ;; (chdir (first-subdirectory ".")), which enters "bin/".
          ;; We need the build root so patchelf finds bin/Runner.Listener
          ;; and the install-plan copies all of the runner files.
          (add-after 'binary-unpack 'fix-cwd
            (lambda _
              (chdir "..")))
          ;; The runner refuses to run from the read-only store: it writes
          ;; run-helper.sh, .credentials, _diag, ... next to its own files.
          ;; So ship launchers that copy the immutable store template into a
          ;; per-user, writable work directory and run the configured script
          ;; from there with a working PATH/LD_LIBRARY_PATH/SSL_CERT_DIR.
          (add-after 'install 'install-launchers
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (define (input->bin name)
                (string-append (assoc-ref inputs name) "/bin"))
              (define (input->dir name subdir)
                (string-append (assoc-ref inputs name) "/" subdir))
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (template (string-append out "/lib/actions-runner"))
                     (path-prefix
                      (string-join
                       (map input->bin
                            '("coreutils" "glibc" "grep"
                              "findutils" "which"))
                       ":"))
                     (glibc-sbin (input->dir "glibc" "sbin"))
                     (ld-path
                      (string-join
                       (map (lambda (name)
                              (input->dir name "lib"))
                            '("gcc" "zlib" "openssl" "icu4c"))
                       ":"))
                     (cert-dir (input->dir "nss-certs"
                                           "etc/ssl/certs")))
                (mkdir-p bin)
                (for-each
                 (lambda (entry)
                   (let* ((name (car entry))
                          (script (cdr entry))
                          (target (string-append bin "/" name))
                          (launcher
                           (string-append
                            "#!/bin/bash\n"
                            "# Generated by Guix.  The GitHub Actions runner\n"
                            "# must run from a writable directory, so copy the\n"
                            "# immutable store template into a per-user work\n"
                            "# directory (override with ACTIONS_RUNNER_DIR).\n\n"
                            "set -e\n\n"
                            "if [ -n \"${ACTIONS_RUNNER_DIR:-}\" ]; then\n"
                            "    RUNNER_DIR=\"$ACTIONS_RUNNER_DIR\"\n"
                            "elif [ -n \"${XDG_DATA_HOME:-}\" ]; then\n"
                            "    RUNNER_DIR=\"$XDG_DATA_HOME/actions-runner\"\n"
                            "elif [ -n \"${HOME:-}\" ]; then\n"
                            "    RUNNER_DIR=\"$HOME/.local/share/actions-runner\"\n"
                            "else\n"
                            "    RUNNER_DIR=\"$PWD/actions-runner\"\n"
                            "fi\n\n"
                            "if [ ! -x \"$RUNNER_DIR/bin/Runner.Listener\" ]; then\n"
                            "    echo \"Initializing GitHub"
                            " Actions runner in $RUNNER_DIR\"\n"
                            "    mkdir -p \"$RUNNER_DIR\"\n"
                            "    cp -a \"" template
                            "/.\" \"$RUNNER_DIR/\"\n"
                            "    chmod -R u+w \"$RUNNER_DIR\"\n"
                            "fi\n\n"
                            "export PATH=\"" path-prefix ":" glibc-sbin ":$PATH\"\n"
                            "export LD_LIBRARY_PATH=\"" ld-path
                            ":${LD_LIBRARY_PATH:-}\"\n"
                            "export SSL_CERT_DIR=\"" cert-dir "\"\n\n"
                            "cd \"$RUNNER_DIR\"\n"
                            "exec \"./" script "\" \"$@\"\n")))
                     (with-output-to-file target
                       (lambda () (display launcher)))
                     (chmod target #o755)))
                 '(("actions-runner" . "run.sh")
                   ("actions-runner-config" . "config.sh")))
                #t))))))
    (inputs
     (list bash-minimal
           coreutils-minimal
           glibc
           grep
           findutils
           which
           nss-certs
           (list gcc "lib")
           icu4c
           openssl
           zlib))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://github.com/actions/runner")
    (synopsis "Self-hosted runner for GitHub Actions")
    (description
     "Actions Runner is the application that runs jobs from GitHub Actions
workflows.  It runs on any x86-64 or ARM64 Linux machine and executes build
jobs dispatched by GitHub Actions.  This package ships a self-contained
distribution with the .NET runtime, shell scripts, and bundled Node.js for
running JavaScript actions.

The @command{actions-runner} and @command{actions-runner-config} launchers
copy the runner into a writable work directory (under
@file{$XDG_DATA_HOME/actions-runner} or
@file{$HOME/.local/share/actions-runner}, overridable with the
@env{ACTIONS_RUNNER_DIR} environment variable) on first use, then run
@command{run.sh} and @command{config.sh} from there.")
    (license license:expat)))