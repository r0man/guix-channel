(define-module (r0man guix packages herdr)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary))

;; herdr is written in Rust, but its build compiles a vendored
;; libghostty-vt with Zig 0.16 and around 300 crates.io dependencies,
;; including a patched portable-pty crate, which makes a from-source
;; package impractical for now.  This package fetches the per-platform
;; prebuilt static binary from upstream's GitHub release instead.

(define %herdr-version "0.9.1")

(define (herdr-binary arch hash)
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/herdrdev/herdr/releases/download/v"
          %herdr-version "/herdr-linux-" arch))
    (sha256 (base32 hash))))

(define-public herdr
  (package
    (name "herdr")
    (version %herdr-version)
    (source
     (let-system system
       (if (string-prefix? "aarch64" system)
           (herdr-binary
            "aarch64" "17ldpldp5ayf4qqaipjq5bn51b9xf2irnglqkaivjb2zfkgg9k7l")
           (herdr-binary
            "x86_64" "1dslbhymcl24sk93q1ddb3fa8b35iw23zm710vq1wrgbdg8zw0ia"))))
    (build-system binary-build-system)
    (arguments
     (list
      ;; The upstream binary is static-pie linked with no interpreter or
      ;; shared library dependencies, so it must not be patchelfed or
      ;; stripped - either would corrupt its layout.
      #:patchelf-plan #~'()
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:install-plan #~'(("herdr" "bin/herdr"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chmod-binary
            (lambda _
              ;; The single-file release keeps its upstream name
              ;; (herdr-linux-x86_64 and friends), which varies per
              ;; architecture; normalize it to "herdr".
              (let ((binary (car (find-files "." "^herdr"))))
                (chmod binary #o755)
                (rename-file binary "herdr")))))))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://herdr.dev")
    (synopsis "Terminal workspace manager for AI coding agents")
    (description
     "Herdr is a terminal workspace manager for AI coding agents.  It keeps
terminals running in a background server when the client is closed or the SSH
connection is lost, and restores the saved layout after a server or machine
restart, resuming supported agent sessions.  Local work and saved SSH machines
live in one window with a combined agent list and independent reconnects.
Every pane is marked working, blocked, or idle, and when an agent stops and
needs an answer, Herdr says so.  Agents drive Herdr through its command line
interface and socket API: they can spawn panes, prompt each other, and wait
until another agent is genuinely blocked.  Herdr runs coding agents such as
Claude Code, Codex, Cursor, OpenCode, and Grok without wrapping or replacing
them, supports tmux-style prefix keys as well as mouse interaction, and can
be extended with plugins.  This package installs the prebuilt static binary
from upstream releases.")
    (license license:asl2.0)))
