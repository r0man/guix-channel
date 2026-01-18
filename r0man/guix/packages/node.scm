(define-module (r0man guix packages node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system node)
  #:use-module (guix gexp))

(define-public node-anthropic-ai-claude-code
  (package
    (name "node-anthropic-ai-claude-code")
    (version "2.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@anthropic-ai/claude-code/"
             "-/claude-code-" version ".tgz"))
       (sha256
        (base32 "1186gyxr7pqninm796hxxiwqvky9jwi9dpym9xmn103gwgxq9lln"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'install 'replace-vendored-ripgrep
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/node_modules"
                                         "/@anthropic-ai/claude-code"))
                     (vendor-rg-dir (string-append lib "/vendor/ripgrep"))
                     (rg-bin (search-input-file inputs "/bin/rg")))
                ;; Delete all vendored ripgrep binaries
                (when (file-exists? vendor-rg-dir)
                  (delete-file-recursively vendor-rg-dir))
                ;; Recreate the expected directory structure with symlinks to Guix ripgrep
                (mkdir-p (string-append vendor-rg-dir "/arm64-linux"))
                (mkdir-p (string-append vendor-rg-dir "/x64-linux"))
                (symlink rg-bin
                         (string-append vendor-rg-dir "/arm64-linux/rg"))
                (symlink rg-bin
                         (string-append vendor-rg-dir "/x64-linux/rg")))))
          (delete 'validate-runpath))))
    (inputs (list ripgrep))
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

(define-public node-zed-industries-claude-code-acp
  (package
    (name "node-zed-industries-claude-code-acp")
    (version "0.12.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@zed-industries/claude-code-acp/"
             "-/claude-code-acp-" version ".tgz"))
       (sha256
        (base32 "0d5q8yvapx4yh725si6fjgbszs6an0s8lab8p6iymca4qabpglxk"))))
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
              (let* ((lib (string-append #$output "/lib/node_modules/@zed-industries/claude-code-acp"))
                     (bin (string-append #$output "/bin")))
                (mkdir-p lib)
                (mkdir-p bin)
                (copy-recursively "dist" (string-append lib "/dist"))
                (copy-file "package.json" (string-append lib "/package.json"))
                (symlink (string-append lib "/dist/index.js")
                         (string-append bin "/claude-code-acp")))))
          (delete 'validate-runpath))))
    (home-page "https://github.com/zed-industries/claude-code-acp")
    (synopsis "ACP adapter for Claude Code integration")
    (description
     "This package provides an ACP (Agent Client Protocol) adapter that
enables Claude Code to work with ACP-compatible clients like Zed, Emacs,
Neovim, and Marimo notebooks.  It supports context mentions, images, tool
execution with permission requests, edit review, TODO lists, interactive
terminals, custom slash commands, and MCP server integration.")
    (license license:asl2.0)))
