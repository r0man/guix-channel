(define-module (r0man guix packages display-managers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages display-managers)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define %artwork-repository
  (let ((commit "a55a0165d0604d588326107f3d474205185fd352"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://git.savannah.gnu.org/git/guix/guix-artwork.git")
            (commit commit)))
      (file-name (string-append "guix-artwork-" (string-take commit 7)
                                "-checkout"))
      (sha256
       (base32
        "06n94liv4d5hlxla37ck99j5zxmfcildhjfxm1r2pj7h8ryyvdy2")))))

(define-public guix-sugar-light-sddm-theme
  (package
    (inherit sugar-light-sddm-theme)
    (name "guix-sugar-light-sddm-theme")
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((out (assoc-ref %outputs "out"))
                 (sddm-themes (string-append out "/share/sddm/themes"))
                 (source (assoc-ref %build-inputs "source"))
                 (target (string-append sddm-themes "/guix-sugar-light")))
            (copy-recursively source ".")
            (delete-file "Background.png")
            (substitute* "theme.conf"
              (("Background.png") "Background.jpg"))
            (copy-file #$(file-append %artwork-repository "/website/static/blog/img/gnu-guix-a-frogs-dream.jpg")
                       "Background.jpg")
            (mkdir-p sddm-themes)
            (copy-recursively "." target)))))))
