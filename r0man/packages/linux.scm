(define-module (r0man packages linux)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux))

(define-public asahi-linux
  (package
    (inherit linux)
    (name "asahi-linux")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/linux.git")
             (commit "6ecde4985ffc8490eb3dbc9303d338b8872e3991")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05r2i3dnwa9v35x93p6r6ixnf456annfx498jgmviwl53jkxi1qc"))))
    (home-page "https://asahilinux.org")
    (synopsis "Linux on Apple Silicon")
    (description "Asahi Linux is a project and community with the goal of porting Linux
to Apple Silicon Macs, starting with the 2020 M1 Mac Mini, MacBook
Air, and MacBook Pro.")))
