(define-module (r0man guix packages wm)
  #:use-module (gnu packages window-management)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public stumpwm-next
  (let ((commit "20d839f2ddfdfd25a8460152bc5dc45a9354e773")
        (revision "0"))
    (package
      (inherit stumpwm)
      (name "stumpwm-next")
      (version (git-version "23.11" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stumpwm/stumpwm")
               (commit commit)))
         (file-name (git-file-name "stumpwm" version))
         (sha256
          (base32 "0b8h33raf0ffl2zv678sxqpvq5xhy6sa88sdm7krnwcd15q8gb85"))))
      (arguments
       (list
        #:asd-systems `'("stumpwm")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'create-asdf-configuration 'build-program
              (lambda* (#:key outputs #:allow-other-keys)
                (build-program
                 (string-append #$output "/bin/stumpwm")
                 outputs
                 #:entry-program '((stumpwm:stumpwm) 0))))
            (add-after 'build-program 'create-desktop-file
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out #$output)
                       (xsessions (string-append out "/share/xsessions")))
                  (mkdir-p xsessions)
                  (call-with-output-file
                      (string-append xsessions "/stumpwm.desktop")
                    (lambda (file)
                      (format file
                              "[Desktop Entry]~@
                        Name=stumpwm~@
                        Comment=The Stump Window Manager~@
                        Exec=~a/bin/stumpwm~@
                        TryExec=~@*~a/bin/stumpwm~@
                        Icon=~@
                        Type=Application~%"
                              out))))))
            (add-after 'create-desktop-file 'install-manual
              (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
                (let* ((out  #$output)
                       (info (string-append out "/share/info")))
                  (invoke "./autogen.sh")
                  (invoke "sh" "./configure" "SHELL=sh")
                  (apply invoke "make" "stumpwm.info" make-flags)
                  (install-file "stumpwm.info" info))))))))))
