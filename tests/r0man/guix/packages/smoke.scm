;;; Smoke test: load every package module in the channel and assert
;;; one known export is a `package?'.  This catches typos, broken
;;; imports, and unused-binding regressions across the whole channel
;;; in one pass — far cheaper than a real `guix build'.

(define-module (test-r0man-guix-packages-smoke)
  #:use-module (guix packages)
  #:use-module (srfi srfi-64))

(test-begin "r0man-guix-packages-smoke")

(define-syntax-rule (test-load-package name module export)
  (test-assert name
    (let ((mod (resolve-interface 'module)))
      (package? (module-ref mod 'export)))))

(test-load-package "bootloader"
                   (r0man guix packages bootloader)
                   gnu-head-bootlogo)

(test-load-package "claude"
                   (r0man guix packages claude)
                   claude-code)

(test-load-package "clojure"
                   (r0man guix packages clojure)
                   babashka)

(test-load-package "container"
                   (r0man guix packages container)
                   container-structure-test)

(test-load-package "cpp"
                   (r0man guix packages cpp)
                   c2ffi-13)

(test-load-package "curl"
                   (r0man guix packages curl)
                   curl-with-libressl)

(test-load-package "display-managers"
                   (r0man guix packages display-managers)
                   guix-sugar-light-sddm-theme)

(test-load-package "emacs"
                   (r0man guix packages emacs)
                   emacs-aider)

(test-load-package "golang-charm"
                   (r0man guix packages golang-charm)
                   go-github-com-clipperhouse-uax29-v2)

(test-load-package "golang-dolthub"
                   (r0man guix packages golang-dolthub)
                   go-cloud-google-com-go-iam)

(test-load-package "golang-maths"
                   (r0man guix packages golang-maths)
                   go-github-com-remyoudompheng-bigfft)

(test-load-package "golang-web"
                   (r0man guix packages golang-web)
                   go-github-com-aws-smithy-go)

(test-load-package "golang-xyz"
                   (r0man guix packages golang-xyz)
                   go-github-com-aleksi-pointer)

(test-load-package "java"
                   (r0man guix packages java)
                   graalvm-ce-21)

(test-load-package "lisp"
                   (r0man guix packages lisp)
                   sbcl-cl-autowrap-next)

(test-load-package "node"
                   (r0man guix packages node)
                   node-anthropic-ai-claude-code)

(test-load-package "pi"
                   (r0man guix packages pi)
                   pi-coding-agent)

(test-load-package "task-management"
                   (r0man guix packages task-management)
                   beads-next)

(test-load-package "wm"
                   (r0man guix packages wm)
                   stumpwm-next)

(test-end "r0man-guix-packages-smoke")
