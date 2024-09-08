(define-module (r0man guix packages display-managers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu artwork)
  #:use-module (gnu packages display-managers)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages))

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
                 (source (assoc-ref %build-inputs "source")))
            (copy-recursively source ".")
            (substitute* "theme.conf"
              (("Background.png")
               #$(file-append %artwork-repository "/website/static/blog/img/gnu-guix-a-frogs-dream.jpg")))
            (mkdir-p sddm-themes)
            (copy-recursively "." (string-append sddm-themes "/guix-sugar-light"))))))))
