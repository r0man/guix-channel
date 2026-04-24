(define-module (r0man guix packages node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system node)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (nonguix build-system binary))

;; Upstream 2.1.119 changed the npm distribution to a thin wrapper that
;; resolves platform-specific precompiled native binaries via
;; optionalDependencies.  Guix's node-build-system cannot resolve those, so
;; this package fetches the per-platform tarball directly and patchelfs the
;; native ELF to use Guix's glibc.
(define-public node-anthropic-ai-claude-code
  (package
    (name "node-anthropic-ai-claude-code")
    (version "2.1.119")
    (source
     (origin
       (method url-fetch)
       (uri (let ((platform (cond ((target-aarch64?) "linux-arm64")
                                  ((target-x86-64?) "linux-x64")
                                  (else "linux-x64"))))
              (string-append
               "https://registry.npmjs.org/@anthropic-ai/claude-code-"
               platform "/-/claude-code-" platform "-" version ".tgz")))
       (sha256
        (base32
         (cond ((target-aarch64?)
                "028ky2zg98hm5hg3kqh0q8q7i78qcyj3r5cgbg8cpdzw58ya563g")
               ((target-x86-64?)
                "1a4wms0idjkjsby9ib08bpcs1vmd8vmi3w01cq4xrh9ghr59b5ra")
               (else
                "1a4wms0idjkjsby9ib08bpcs1vmd8vmi3w01cq4xrh9ghr59b5ra"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan #~'(("claude"))
      #:install-plan #~'(("." "lib/claude-code/"))
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

(define-public node-zod
  (package
    (name "node-zod")
    (version "3.25.67")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/zod/-/zod-" version ".tgz"))
       (sha256
        (base32 "0ciizc2mvhw0n3wa7sjlb0fzlrxfvlivjqmk0n7dpb7w1pkl0dpi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda _
              (let ((lib (string-append #$output "/lib/node_modules/zod")))
                (mkdir-p lib)
                (copy-recursively "." lib))))
          (delete 'validate-runpath))))
    (home-page "https://zod.dev")
    (synopsis "TypeScript-first schema validation with static type inference")
    (description
     "Zod is a TypeScript-first schema declaration and validation library.")
    (license license:expat)))

(define-public node-agentclientprotocol-sdk
  (package
    (name "node-agentclientprotocol-sdk")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@agentclientprotocol/sdk/"
             "-/sdk-" version ".tgz"))
       (sha256
        (base32 "068hi2k62xal0bfc0pjs7i51b0m7818lkn2jhqxsvi83176z0pi6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda _
              (let* ((lib (string-append #$output
                                         "/lib/node_modules"
                                         "/@agentclientprotocol/sdk"))
                     (nm (string-append lib "/node_modules")))
                (mkdir-p lib)
                (copy-recursively "." lib)
                (mkdir-p nm)
                (symlink (string-append #$node-zod
                                        "/lib/node_modules/zod")
                         (string-append nm "/zod")))))
          (delete 'validate-runpath))))
    (inputs (list node-zod))
    (home-page "https://github.com/anthropics/agent-client-protocol")
    (synopsis "SDK for the Agent Client Protocol")
    (description
     "SDK for building ACP (Agent Client Protocol) compatible agents and
clients.")
    (license license:expat)))

(define-public node-anthropic-ai-claude-agent-sdk
  (package
    (name "node-anthropic-ai-claude-agent-sdk")
    (version "0.2.71")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@anthropic-ai/claude-agent-sdk/"
             "-/claude-agent-sdk-" version ".tgz"))
       (sha256
        (base32 "06v6qrwq55sdbvs39wl8dlyjr3n1ik2c84iscsbsfm8sdbxayzsd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((lib (string-append #$output
                                         "/lib/node_modules"
                                         "/@anthropic-ai/claude-agent-sdk"))
                     (nm (string-append lib "/node_modules"))
                     (vendor-rg-dir (string-append lib "/vendor/ripgrep"))
                     (rg-bin (search-input-file inputs "/bin/rg")))
                (mkdir-p lib)
                (copy-recursively "." lib)
                ;; Symlink zod peer dependency
                (mkdir-p nm)
                (symlink (string-append #$node-zod
                                        "/lib/node_modules/zod")
                         (string-append nm "/zod"))
                ;; Replace vendored ripgrep with Guix ripgrep
                (when (file-exists? vendor-rg-dir)
                  (delete-file-recursively vendor-rg-dir))
                (mkdir-p (string-append vendor-rg-dir "/arm64-linux"))
                (mkdir-p (string-append vendor-rg-dir "/x64-linux"))
                (symlink rg-bin
                         (string-append vendor-rg-dir "/arm64-linux/rg"))
                (symlink rg-bin
                         (string-append vendor-rg-dir "/x64-linux/rg")))))
          (delete 'validate-runpath))))
    (inputs (list node-zod ripgrep))
    (home-page "https://github.com/anthropics/claude-code")
    (synopsis "Claude Agent SDK for TypeScript")
    (description
     "The Claude Agent SDK provides programmatic access to Claude Code's
agentic capabilities for building custom AI coding agents.")
    (license (license:non-copyleft
              "https://github.com/anthropics/claude-code/blob/main/LICENSE.md"
              "See LICENSE.md in the repository."))))

(define-public node-zed-industries-claude-agent-acp
  (package
    (name "node-zed-industries-claude-agent-acp")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@zed-industries/claude-agent-acp/"
             "-/claude-agent-acp-" version ".tgz"))
       (sha256
        (base32 "17z5dwiqh4pg8hb41ac16cgrsxc5pfpmv9scrqlc6fg34cfw8331"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((lib (string-append #$output
                                         "/lib/node_modules"
                                         "/@zed-industries/claude-agent-acp"))
                     (bin (string-append #$output "/bin"))
                     (nm (string-append lib "/node_modules")))
                (mkdir-p lib)
                (mkdir-p bin)
                (copy-recursively "dist" (string-append lib "/dist"))
                (copy-file "package.json" (string-append lib "/package.json"))
                (chmod (string-append lib "/dist/index.js") #o555)
                (symlink (string-append lib "/dist/index.js")
                         (string-append bin "/claude-agent-acp"))
                ;; Symlink runtime dependencies into node_modules
                (mkdir-p (string-append nm "/@agentclientprotocol"))
                (mkdir-p (string-append nm "/@anthropic-ai"))
                (symlink (string-append (assoc-ref inputs "node-zod")
                                        "/lib/node_modules/zod")
                         (string-append nm "/zod"))
                (symlink (string-append (assoc-ref inputs "node-agentclientprotocol-sdk")
                                        "/lib/node_modules/@agentclientprotocol/sdk")
                         (string-append nm "/@agentclientprotocol/sdk"))
                (symlink (string-append (assoc-ref inputs "node-anthropic-ai-claude-agent-sdk")
                                        "/lib/node_modules/@anthropic-ai/claude-agent-sdk")
                         (string-append nm "/@anthropic-ai/claude-agent-sdk")))))
          (delete 'validate-runpath))))
    (inputs (list node-zod
                  node-agentclientprotocol-sdk
                  node-anthropic-ai-claude-agent-sdk))
    (home-page "https://github.com/zed-industries/claude-agent-acp")
    (synopsis "ACP adapter for Claude Code integration")
    (description
     "This package provides an ACP (Agent Client Protocol) adapter that
enables Claude Code to work with ACP-compatible clients like Zed, Emacs,
Neovim, and Marimo notebooks.  It supports context mentions, images, tool
execution with permission requests, edit review, TODO lists, interactive
terminals, custom slash commands, and MCP server integration.")
    (license license:asl2.0)))
