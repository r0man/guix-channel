;;; Copyright © 2024 Murilo <murilo@disroot.org>
;;; Copyright © 2026 Roman Scherer <roman.scherer@burningswell.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; The ghostty and ghostty-latest definitions below are adapted from the
;;; Saayix channel:
;;;   https://codeberg.org/look/saayix/src/branch/main/modules/saayix/packages/terminals.scm

(define-module (r0man guix packages terminals)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zig)
  #:use-module (guix build-system zig)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ghostty
  (package
    (name "ghostty")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ghostty-org/ghostty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d064l17drqcf6bc27jmjxak0n2xqp2mpalakwp3j9mx8yclrmzr"))))
    (build-system zig-build-system)
    (arguments
     (list #:tests? #f
           #:zig zig-0.15
           #:install-source? #f
           #:zig-release-type "fast"
           #:zig-build-flags
           #~(list "-Dcpu=baseline"
                   "-Drenderer=opengl"
                   "--prefix" "."
                   "--system"
                   (string-append (getenv "TMPDIR") "/source/zig-cache")
                   "--search-prefix"
                   #$(this-package-input "libadwaita")
                   "--search-prefix"
                   #$(this-package-input "gtk4-layer-shell")
                   "--search-prefix"
                   (string-append (getenv "TMPDIR") "/source/bzip2")
                   "-fno-sys=oniguruma")
           #:modules
           '((guix build zig-build-system)
             (guix build utils)
             (ice-9 match))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack-dependencies
                 (lambda _
                   (mkdir-p "bzip2/lib")
                   (symlink
                    (string-append #$(this-package-input "bzip2")
                                   "/lib/libbz2.so")
                    "bzip2/lib/libbzip2.so")))
               (add-after 'install 'fix-desktop-file
                 (lambda _
                   (let ((desktop (string-append
                                   #$output
                                   "/share/applications/com.mitchellh.ghostty.desktop")))
                     (when (file-exists? desktop)
                       (substitute* desktop
                         (("(TryExec|Exec)=\\./bin/ghostty" _ key)
                          (string-append key "=" #$output "/bin/ghostty")))))))
               (add-after 'unpack 'unpack-zig
                 (lambda _
                   (for-each
                    (match-lambda
                      ((dst src)
                       (let* ((dest (string-append "zig-cache/" dst)))
                         (mkdir-p dest)
                         (cond
                          ((string-contains src ".tar.gz")
                           (invoke "tar" "-xf" src "-C" dest "--strip-components=1"))
                          ((string-contains src ".tgz")
                           (invoke "tar" "-xf" src "-C" dest "--strip-components=1"))
                          ((string-contains src ".tar.xz")
                           (invoke "tar" "-xf" src "-C" dest "--strip-components=1"))
                          ((string-contains src ".tar.zst")
                           (invoke "tar" "-xf" src "-C" dest "--strip-components=1"))
                          (else (copy-recursively src dest))))))
                    `(("N-V-__8AAB0eQwD-0MdOEBmz7intriBReIsIDNlukNVoNu6o"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/zlib-1220fed0c74e1019b3ee29edae2051788b080cd96e90d56836eea857b0b966742efb.tar.gz")
                          (sha256
                           (base32
                            "0p6h2i9ajdp46lckdpibfqy4vz5nh5r22bqq96mp41k0ydiqis0p")))))
                      ("zigimg-0.1.0-8_eo2vHnEwCIVW34Q14Ec-xUlzIoVg86-7FU2ypPtxms"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://github.com/ivanstepanovftw/zigimg/archive/d7b7ab0ba0899643831ef042bd73289510b39906.tar.gz")
                          (sha256
                           (base32
                            "0ly53dd3pj8hl3kkf3h8x4dw79yb7riwj9qc9da18mdkl9mxf7ic")))))
                      ("wayland-0.5.0-dev-lQa1khrMAQDJDwYFKpdH3HizherB7sHo5dKMECfvxQHe"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/zig_wayland-1b5c038ec10da20ed3a15b0b2a6db1c21383e8ea.tar.gz")
                          (sha256
                           (base32
                            "0khjg5q1z1d4sgnyhfjqzb8c6wizx79p3gz343sjgmfhbrrnn52g")))))
                      ("zig_objc-0.0.0-Ir_Sp5gTAQCvxxR7oVIrPXxXwsfKgVP7_wqoOQrZjFeK"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/zig_objc-f356ed02833f0f1b8e84d50bed9e807bf7cdc0ae.tar.gz")
                          (sha256
                           (base32
                            "1k4fq05brsm799qpkxbwcq1dgs5jyc4hkcrcfb6nyd95frrsz16x")))))
                      ("zig_js-0.0.0-rjCAV-6GAADxFug7rDmPH-uM_XcnJ5NmuAMJCAscMjhi"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/zig_js-04db83c617da1956ac5adc1cb9ba1e434c1cb6fd.tar.gz")
                          (sha256
                           (base32
                            "18vkzib7xgvk4g1xk18070w3yfg9kjnqc0q9p029blqmc3jih82c")))))
                      ("zf-0.10.3-OIRy8RuJAACKA3Lohoumrt85nRbHwbpMcUaLES8vxDnh"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/zf-3c52637b7e937c5ae61fd679717da3e276765b23.tar.gz")
                          (sha256
                           (base32
                            "0s25gjvp7rns1l52jvgbd7aakndlvfs5xh9b4wk9wkphia95s09v")))))
                      ("z2d-0.10.0-j5P_Hu-6FgBsZNgwphIqh17jDnj8_yPtD8yzjO6PpHRQ"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/z2d-0.10.0-j5P_Hu-6FgBsZNgwphIqh17jDnj8_yPtD8yzjO6PpHRQ.tar.gz")
                          (sha256
                           (base32
                            "1xwpcw2awxf2r1kz27m0j4pzpi5g92gd1i2mzqvhkvnmxyi1vwk9")))))
                      ("N-V-__8AAAzZywE3s51XfsLbP9eyEw57ae9swYB9aGB6fCMs"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/wuffs-122037b39d577ec2db3fd7b2130e7b69ef6cc1807d68607a7c232c958315d381b5cd.tar.gz")
                          (sha256
                           (base32
                            "04qwpr8c4xjla4skwb1fpvkjc0c611qhbhz9xp3c9rlnpq5d4k4y")))))
                      ("N-V-__8AAKw-DAAaV8bOAAGqA0-oD7o-HNIlPFYKRXSPT03S"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/wayland-protocols-258d8f88f2c8c25a830c6316f87d23ce1a0f12d9.tar.gz")
                          (sha256
                           (base32
                            "1y1h0pmql53x6ixbsycgkzxlxsxqs9fkps754c7ycx8vx3fwmvaw")))))
                      ("N-V-__8AAKrHGAAs2shYq8UkE6bGcR1QJtLTyOE_lcosMn6t"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/wayland-9cb3d7aa9dc995ffafdbdef7ab86a949d0fb0e7d.tar.gz")
                          (sha256
                           (base32
                            "03f574n5w0y6glr7lf8xjd71844qh8kxxb1s3zjpfxj3ivb92hga")))))
                      ("vaxis-0.5.1-BWNV_LosCQAGmCCNOLljCIw6j6-yt53tji6n6rwJ2BhS"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/vaxis-7dbb9fd3122e4ffad262dd7c151d80d863b68558.tar.gz")
                          (sha256
                           (base32
                            "1xlf12dlzda0z4d3svq0qibvfgqzkrv4igg6qqg58nwwr0mk6wif")))))
                      ("uucode-0.2.0-ZZjBPqZVVABQepOqZHR7vV_NcaN-wats0IB6o-Exj6m9"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/uucode-0.2.0-ZZjBPqZVVABQepOqZHR7vV_NcaN-wats0IB6o-Exj6m9.tar.gz")
                          (sha256
                           (base32
                            "15az8qzp0rg5qj8ma0dam9j8jbf4wwb7wxsiq3iymmlb9w7yxayh")))))
                      ("uucode-0.1.0-ZZjBPj96QADXyt5sqwBJUnhaDYs_qBeeKijZvlRa0eqM"
                       (ungexp
                        (origin
                          (method git-fetch)
                          (uri (git-reference
                                (url "https://github.com/jacobsandlund/uucode")
                                (commit "5f05f8f83a75caea201f12cc8ea32a2d82ea9732")))
                          (file-name "uucode")
                          (sha256
                           (base32
                            "1zrdyhnqs0v46qasxb2kwd7694j8r8z6w4zlnfp42x8j6kwy2wxh")))))
                      ("N-V-__8AAHffAgDU0YQmynL8K35WzkcnMUmBVQHQ0jlcKpjH"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/utfcpp-1220d4d18426ca72fc2b7e56ce47273149815501d0d2395c2a98c726b31ba931e641.tar.gz")
                          (sha256
                           (base32
                            "1ksrdf7dy4csazhddi64xahks8jzf4r8phgkjg9hfxp722iniipz")))))
                      ("N-V-__8AANb6pwD7O1WG6L5nvD_rNMvnSc9Cpg1ijSlTYywv"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/spirv_cross-1220fb3b5586e8be67bc3feb34cbe749cf42a60d628d2953632c2f8141302748c8da.tar.gz")
                          (sha256
                           (base32
                            "1qspcsx56v0mddarb6f05i748wsl2ln3d8863ydsczsyqk7nyaxm")))))
                      ("N-V-__8AAPlZGwBEa-gxrcypGBZ2R8Bse4JYSfo_ul8i2jlG"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/sentry-1220446be831adcca918167647c06c7b825849fa3fba5f22da394667974537a9c77e.tar.gz")
                          (sha256
                           (base32
                            "1pqqqcin8nw398rvn187dfqlab4vikdssiry14qqs6nnr1y4kiia")))))
                      ("N-V-__8AAKYZBAB-CFHBKs3u4JkeiT4BMvyHu3Y5aaWF3Bbs"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/plasma_wayland_protocols-12207e0851c12acdeee0991e893e0132fc87bb763969a585dc16ecca33e88334c566.tar.gz")
                          (sha256
                           (base32
                            "0hgl1p173pxs50z1p6mjjzcqssn44aq0ip166k56p3nd98hvln2w")))))
                      ("N-V-__8AADYiAAB_80AWnH1AxXC0tql9thT-R-DYO1gBqTLc"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/pixels-12207ff340169c7d40c570b4b6a97db614fe47e0d83b5801a932dcd44917424c8806.tar.gz")
                          (sha256
                           (base32
                            "06pi3f3lhyxfzczhwrc2b4n0jhhzydbz96qlpw12a24is0b3ps2m")))))
                      ("N-V-__8AAHjwMQDBXnLq3Q2QhaivE0kE2aD138vtX2Bq1g7c"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/oniguruma-1220c15e72eadd0d9085a8af134904d9a0f5dfcbed5f606ad60edc60ebeccd9706bb.tar.gz")
                          (sha256
                           (base32
                            "187jk4fxdkzc0wrcx4kdy4v6p1snwmv8r97i1d68yi3q5qha26h0")))))
                      ("N-V-__8AAMVLTABmYkLqhZPLXnMl-KyN38R8UVYqGrxqO26s"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/NerdFontsSymbolsOnly-3.4.0.tar.gz")
                          (sha256
                           (base32
                            "010d7gkv359qg555d89i4hhgb56c8f69kw5jsx4f5gflaswx2r0i")))))
                      ("N-V-__8AAG3RoQEyRC2Vw7Qoro5SYBf62IHn3HjqtNVY6aWK"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/libxml2-2.11.5.tar.gz")
                          (sha256
                           (base32
                            "05b2kbccbkb5pkizwx2s170lcqvaj7iqjr5injsl5sry5sg0aa3c")))))
                      ("libxev-0.0.0-86vtc4IcEwCqEYxEYoN_3KXmc6A9VLcm22aVImfvecYs"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/libxev-34fa50878aec6e5fa8f532867001ab3c36fae23e.tar.gz")
                          (sha256
                           (base32
                            "1mvx91wn7499xfx76fxijq4x66x1g5yk4cpr52hii9g4jrmyl0v0")))))
                      ("N-V-__8AAJrvXQCqAT8Mg9o_tk6m0yf5Fz-gCNEOKLyTSerD"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/libpng-1220aa013f0c83da3fb64ea6d327f9173fa008d10e28bc9349eac3463457723b1c66.tar.gz")
                          (sha256
                           (base32
                            "0fm0y7543w2gx5sz3zg9i46x1am51c77a554r0zqwpphdjs9bk7y")))))
                      ("N-V-__8AAIC5lwAVPJJzxnCAahSvZTIlG-HhtOvnM1uh-66x"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/JetBrainsMono-2.304.tar.gz")
                          (sha256
                           (base32
                            "1i2w213919avi0apgbw720wqy0z46a89bwv3b65hkbc2icg6jyn5")))))
                      ("N-V-__8AABVbAwBwDRyZONfx553tvMW8_A2OKUoLzPUSRiLF"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/ghostty-themes-release-20260216-151611-fc73ce3.tgz")
                          (sha256
                           (base32
                            "1zd81af7hjnyfq3dypl3xg9bd5mkh2r01m89jsv4m08cdaw0n80l")))))
                      ("N-V-__8AAEbOfQBnvcFcCX2W5z7tDaN8vaNZGamEQtNOe0UI"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://github.com/ocornut/imgui/archive/refs/tags/v1.92.5-docking.tar.gz")
                          (sha256
                           (base32
                            "1jzr65gpx4mqfcdbnf2rm2kd20jmj9whwdb7x1df3wvmih7c45n8")))))
                      ("N-V-__8AAGmZhABbsPJLfbqrh6JTHsXhY6qCaLAQyx25e0XE"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/highway-66486a10623fa0d72fe91260f96c892e41aceb06.tar.gz")
                          (sha256
                           (base32
                            "04m21b46h6c4x099r9qb720ql9llpzz8yq3k94i8zq7l7s4zim47")))))
                      ("N-V-__8AAG02ugUcWec-Ndp-i7JTsJ0dgF8nnJRUInkGLG7G"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/harfbuzz-11.0.0.tar.xz")
                          (sha256
                           (base32
                            "16rb7aazy36pj3xrjy149dd90j9yv7q5jnqx5kz2air1zsx52qzi")))))
                      ("N-V-__8AALiNBAA-_0gprYr92CjrMj1I5bqNu0TSJOnjFNSr"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/gtk4-layer-shell-1.1.0.tar.gz")
                          (sha256
                           (base32
                            "12396gx723ybgq1xp9i02257hsmzqhb5z9b39xdyypha4s0l4a4q")))))
                      ("gobject-0.3.0-Skun7ANLnwDvEfIpVmohcppXgOvg_I6YOJFmPIsKfXk-"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/gobject-2025-11-08-23-1.tar.zst")
                          (sha256
                           (base32
                            "0j0csvsyvp0193mpkdp25s14kargppmdyslbhi5qw788y0347gfr")))))
                      ("N-V-__8AABzkUgISeKGgXAzgtutgJsZc0-kkeqBBscJgMkvy"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/glslang-12201278a1a05c0ce0b6eb6026c65cd3e9247aa041b1c260324bf29cee559dd23ba1.tar.gz")
                          (sha256
                           (base32
                            "1dcpm70fhxk07vk37f5l0hb9gxfv6pjgbqskk8dfbcwwa2xyv8hl")))))
                      ("N-V-__8AADcZkgn4cMhTUpIz6mShCKyqqB-NBtf_S2bHaTC-"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/gettext-0.24.tar.gz")
                          (sha256
                           (base32
                            "1dqq2ln01mfwr4gblvy0cyvarbqnv09ml5sdhksdlw1xb4ym0669")))))
                      ("N-V-__8AAKLKpwC4H27Ps_0iL3bPkQb-z6ZVSrB-x_3EEkub"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/freetype-1220b81f6ecfb3fd222f76cf9106fecfa6554ab07ec7fdc4124b9bb063ae2adf969d.tar.gz")
                          (sha256
                           (base32
                            "035r5bypzapa1x7za7lpvpkz58fxynz4anqzbk8705hmspsh2wj2")))))
                      ("N-V-__8AAIrfdwARSa-zMmxWwFuwpXf1T3asIN7s5jqi9c1v"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/fontconfig-2.14.2.tar.gz")
                          (sha256
                           (base32
                            "0mcarq6v9k7k9a8is23vq9as0niv0hbagwdabknaq6472n9dv8iv")))))
                      ("N-V-__8AALw2uwF_03u4JRkZwRLc3Y9hakkYV7NKRR9-RIZJ"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/breakpad-b99f444ba5f6b98cac261cbb391d8766b34a5918.tar.gz")
                          (sha256
                           (base32
                            "1nbadlml3r982bz1wyp17w33hngzkb07f47nrrk0g68s7na9ijkc")))))
                      ("N-V-__8AANT61wB--nJ95Gj_ctmzAtcjloZ__hRqNw5lC1Kr"
                       (ungexp
                        (origin
                          (method url-fetch)
                          (uri "https://deps.files.ghostty.org/DearBindings_v0.17_ImGui_v1.92.5-docking.tar.gz")
                          (sha256
                           (base32
                            "18xsf0zisr9q8xqa8g3kh4qkqb9rrikkvqiyhmwzc9h9w00cbzlb"))))))))))))
    (native-inputs
     (list `(,glib "bin")
           blueprint-compiler
           gnu-gettext
           gobject-introspection
           ncurses
           pandoc
           pkg-config
           tar))
    (inputs
     (list bzip2
           expat
           fontconfig
           freetype
           glslang
           gtk4-layer-shell
           harfbuzz
           libadwaita
           libglvnd
           libpng
           libx11
           libxcursor
           libxi
           libxrandr
           zlib))
    (native-search-paths
     ;; FIXME: This should only be located in 'ncurses'.  Nonetheless it is
     ;; provided for usability reasons.  See <https://bugs.gnu.org/22138>.
     (list (search-path-specification
            (variable "TERMINFO_DIRS")
            (files '("share/terminfo")))))
    (synopsis "Fast, native, feature-rich terminal emulator")
    (description "Ghostty is a terminal emulator that differentiates itself by
being fast, feature-rich, and native.  While there are many excellent terminal
emulators available, they all force you to choose between speed, features, or
native UIs.  Ghostty provides all three.")
    (home-page "https://ghostty.org")
    (license license:expat)))

(define-public ghostty-latest
  ;; This commit matches the ghostty pin in dakra/ghostel build.zig.zon, so
  ;; emacs-ghostel builds against the matching libghostty-vt API.
  (let* ((commit "01825411ab2720e47e6902e9464e805bc6a062a1")
         (revision "3"))
    (package/inherit ghostty
      (name "ghostty-latest")
      (version (git-version (package-version ghostty) revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ghostty-org/ghostty")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1vciyvbszkhrcdn72vx9c1m1dqlc8fid3cv8gb6zfa2xnc0qhcyc"))))
      (arguments
       (list #:tests? #f
             #:zig zig-0.15
             #:install-source? #f
             #:zig-release-type "fast"
             #:zig-build-flags
             #~(list "-Dcpu=baseline"
                     "-Drenderer=opengl"
                     "-Dgtk-x11=false"
                     "-Dgtk-wayland=true"
                     "--prefix" "."
                     "--system" (string-append (getenv "TMPDIR") "/source/zig-cache")
                     "--search-prefix" #$(this-package-input "libadwaita")
                     "--search-prefix" #$(this-package-input "gtk4-layer-shell")
                     "--search-prefix" (string-append (getenv "TMPDIR") "/source/bzip2")
                     "-fno-sys=oniguruma")
             #:modules
             '((guix build zig-build-system)
               (guix build utils)
               (ice-9 match))
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'unpack-dependencies
                   (lambda _
                     (mkdir-p "bzip2/lib")
                     (symlink
                      (string-append #$(this-package-input "bzip2")
                                     "/lib/libbz2.so")
                      "bzip2/lib/libbzip2.so")))
                 (add-after 'install 'fix-desktop-file
                   (lambda _
                     (let ((desktop (string-append
                                     #$output
                                     "/share/applications/com.mitchellh.ghostty.desktop")))
                       (when (file-exists? desktop)
                         (substitute* desktop
                           (("(TryExec|Exec)=\\./bin/ghostty" _ key)
                            (string-append key "=" #$output "/bin/ghostty")))))))
                 (add-after 'unpack 'unpack-zig
                   (lambda _
                     (for-each
                      (match-lambda
                        ((dst src)
                         (let* ((dest (string-append "zig-cache/" dst)))
                           (mkdir-p dest)
                           (cond
                            ((string-contains src ".tar.gz")
                             (invoke "tar" "-xf" src "-C" dest "--strip-components=1"))
                            ((string-contains src ".tgz")
                             (invoke "tar" "-xf" src "-C" dest "--strip-components=1"))
                            ((string-contains src ".tar.xz")
                             (invoke "tar" "-xf" src "-C" dest "--strip-components=1"))
                            ((string-contains src ".tar.zst")
                             (invoke "tar" "-xf" src "-C" dest "--strip-components=1"))
                            (else (copy-recursively src dest))))))
                      `(("N-V-__8AAB0eQwD-0MdOEBmz7intriBReIsIDNlukNVoNu6o"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/zlib-1220fed0c74e1019b3ee29edae2051788b080cd96e90d56836eea857b0b966742efb.tar.gz")
                            (sha256 (base32 "0p6h2i9ajdp46lckdpibfqy4vz5nh5r22bqq96mp41k0ydiqis0p")))))
                        ("zigimg-0.1.0-8_eo2vHnEwCIVW34Q14Ec-xUlzIoVg86-7FU2ypPtxms"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://github.com/ivanstepanovftw/zigimg/archive/d7b7ab0ba0899643831ef042bd73289510b39906.tar.gz")
                            (sha256 (base32 "0ly53dd3pj8hl3kkf3h8x4dw79yb7riwj9qc9da18mdkl9mxf7ic")))))
                        ("wayland-0.5.0-dev-lQa1khrMAQDJDwYFKpdH3HizherB7sHo5dKMECfvxQHe"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/zig_wayland-1b5c038ec10da20ed3a15b0b2a6db1c21383e8ea.tar.gz")
                            (sha256 (base32 "0khjg5q1z1d4sgnyhfjqzb8c6wizx79p3gz343sjgmfhbrrnn52g")))))
                        ("zig_objc-0.0.0-Ir_Sp5gTAQCvxxR7oVIrPXxXwsfKgVP7_wqoOQrZjFeK"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/zig_objc-f356ed02833f0f1b8e84d50bed9e807bf7cdc0ae.tar.gz")
                            (sha256 (base32 "1k4fq05brsm799qpkxbwcq1dgs5jyc4hkcrcfb6nyd95frrsz16x")))))
                        ("zig_js-0.0.0-rjCAV-6GAADxFug7rDmPH-uM_XcnJ5NmuAMJCAscMjhi"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/zig_js-04db83c617da1956ac5adc1cb9ba1e434c1cb6fd.tar.gz")
                            (sha256 (base32 "18vkzib7xgvk4g1xk18070w3yfg9kjnqc0q9p029blqmc3jih82c")))))
                        ("zf-0.10.3-OIRy8RuJAACKA3Lohoumrt85nRbHwbpMcUaLES8vxDnh"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/zf-3c52637b7e937c5ae61fd679717da3e276765b23.tar.gz")
                            (sha256 (base32 "0s25gjvp7rns1l52jvgbd7aakndlvfs5xh9b4wk9wkphia95s09v")))))
                        ("z2d-0.10.0-j5P_Hu-6FgBsZNgwphIqh17jDnj8_yPtD8yzjO6PpHRQ"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/z2d-0.10.0-j5P_Hu-6FgBsZNgwphIqh17jDnj8_yPtD8yzjO6PpHRQ.tar.gz")
                            (sha256 (base32 "1xwpcw2awxf2r1kz27m0j4pzpi5g92gd1i2mzqvhkvnmxyi1vwk9")))))
                        ("N-V-__8AAAzZywE3s51XfsLbP9eyEw57ae9swYB9aGB6fCMs"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/wuffs-122037b39d577ec2db3fd7b2130e7b69ef6cc1807d68607a7c232c958315d381b5cd.tar.gz")
                            (sha256 (base32 "04qwpr8c4xjla4skwb1fpvkjc0c611qhbhz9xp3c9rlnpq5d4k4y")))))
                        ("N-V-__8AAFdWDwA0ktbNUi9pFBHCRN4weXIgIfCrVjfGxqgA"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://gitlab.freedesktop.org/wayland/wayland-protocols/-/archive/1.47/wayland-protocols-1.47.tar.gz")
                            (sha256 (base32 "08rjsdh8w0lpj2pr83rziw0yxh4szfsw9p5fg8jkh47lnm5g2bfx")))))
                        ("N-V-__8AAKw-DAAaV8bOAAGqA0-oD7o-HNIlPFYKRXSPT03S"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/wayland-protocols-258d8f88f2c8c25a830c6316f87d23ce1a0f12d9.tar.gz")
                            (sha256 (base32 "1y1h0pmql53x6ixbsycgkzxlxsxqs9fkps754c7ycx8vx3fwmvaw")))))
                        ("N-V-__8AAKrHGAAs2shYq8UkE6bGcR1QJtLTyOE_lcosMn6t"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/wayland-9cb3d7aa9dc995ffafdbdef7ab86a949d0fb0e7d.tar.gz")
                            (sha256 (base32 "03f574n5w0y6glr7lf8xjd71844qh8kxxb1s3zjpfxj3ivb92hga")))))
                        ("vaxis-0.5.1-BWNV_LosCQAGmCCNOLljCIw6j6-yt53tji6n6rwJ2BhS"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/vaxis-7dbb9fd3122e4ffad262dd7c151d80d863b68558.tar.gz")
                            (sha256 (base32 "1xlf12dlzda0z4d3svq0qibvfgqzkrv4igg6qqg58nwwr0mk6wif")))))
                        ("uucode-0.2.0-ZZjBPqZVVABQepOqZHR7vV_NcaN-wats0IB6o-Exj6m9"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/uucode-0.2.0-ZZjBPqZVVABQepOqZHR7vV_NcaN-wats0IB6o-Exj6m9.tar.gz")
                            (sha256 (base32 "15az8qzp0rg5qj8ma0dam9j8jbf4wwb7wxsiq3iymmlb9w7yxayh")))))
                        ("uucode-0.1.0-ZZjBPj96QADXyt5sqwBJUnhaDYs_qBeeKijZvlRa0eqM"
                         (ungexp
                          (origin
                            (method git-fetch)
                            (uri (git-reference
                                  (url "https://github.com/jacobsandlund/uucode")
                                  (commit "5f05f8f83a75caea201f12cc8ea32a2d82ea9732")))
                            (file-name "uucode")
                            (sha256 (base32 "1zrdyhnqs0v46qasxb2kwd7694j8r8z6w4zlnfp42x8j6kwy2wxh")))))
                        ("N-V-__8AAHffAgDU0YQmynL8K35WzkcnMUmBVQHQ0jlcKpjH"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/utfcpp-1220d4d18426ca72fc2b7e56ce47273149815501d0d2395c2a98c726b31ba931e641.tar.gz")
                            (sha256 (base32 "1ksrdf7dy4csazhddi64xahks8jzf4r8phgkjg9hfxp722iniipz")))))
                        ("N-V-__8AANb6pwD7O1WG6L5nvD_rNMvnSc9Cpg1ijSlTYywv"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/spirv_cross-1220fb3b5586e8be67bc3feb34cbe749cf42a60d628d2953632c2f8141302748c8da.tar.gz")
                            (sha256 (base32 "1qspcsx56v0mddarb6f05i748wsl2ln3d8863ydsczsyqk7nyaxm")))))
                        ("N-V-__8AAPlZGwBEa-gxrcypGBZ2R8Bse4JYSfo_ul8i2jlG"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/sentry-1220446be831adcca918167647c06c7b825849fa3fba5f22da394667974537a9c77e.tar.gz")
                            (sha256 (base32 "1pqqqcin8nw398rvn187dfqlab4vikdssiry14qqs6nnr1y4kiia")))))
                        ("N-V-__8AAKYZBAB-CFHBKs3u4JkeiT4BMvyHu3Y5aaWF3Bbs"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/plasma_wayland_protocols-12207e0851c12acdeee0991e893e0132fc87bb763969a585dc16ecca33e88334c566.tar.gz")
                            (sha256 (base32 "0hgl1p173pxs50z1p6mjjzcqssn44aq0ip166k56p3nd98hvln2w")))))
                        ("N-V-__8AADYiAAB_80AWnH1AxXC0tql9thT-R-DYO1gBqTLc"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/pixels-12207ff340169c7d40c570b4b6a97db614fe47e0d83b5801a932dcd44917424c8806.tar.gz")
                            (sha256 (base32 "06pi3f3lhyxfzczhwrc2b4n0jhhzydbz96qlpw12a24is0b3ps2m")))))
                        ("N-V-__8AAHjwMQDBXnLq3Q2QhaivE0kE2aD138vtX2Bq1g7c"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/oniguruma-1220c15e72eadd0d9085a8af134904d9a0f5dfcbed5f606ad60edc60ebeccd9706bb.tar.gz")
                            (sha256 (base32 "187jk4fxdkzc0wrcx4kdy4v6p1snwmv8r97i1d68yi3q5qha26h0")))))
                        ("N-V-__8AAMVLTABmYkLqhZPLXnMl-KyN38R8UVYqGrxqO26s"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/NerdFontsSymbolsOnly-3.4.0.tar.gz")
                            (sha256 (base32 "010d7gkv359qg555d89i4hhgb56c8f69kw5jsx4f5gflaswx2r0i")))))
                        ("N-V-__8AAG3RoQEyRC2Vw7Qoro5SYBf62IHn3HjqtNVY6aWK"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/libxml2-2.11.5.tar.gz")
                            (sha256 (base32 "05b2kbccbkb5pkizwx2s170lcqvaj7iqjr5injsl5sry5sg0aa3c")))))
                        ("libxev-0.0.0-86vtc4IcEwCqEYxEYoN_3KXmc6A9VLcm22aVImfvecYs"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/libxev-34fa50878aec6e5fa8f532867001ab3c36fae23e.tar.gz")
                            (sha256 (base32 "1mvx91wn7499xfx76fxijq4x66x1g5yk4cpr52hii9g4jrmyl0v0")))))
                        ("N-V-__8AAJrvXQCqAT8Mg9o_tk6m0yf5Fz-gCNEOKLyTSerD"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/libpng-1220aa013f0c83da3fb64ea6d327f9173fa008d10e28bc9349eac3463457723b1c66.tar.gz")
                            (sha256 (base32 "0fm0y7543w2gx5sz3zg9i46x1am51c77a554r0zqwpphdjs9bk7y")))))
                        ("N-V-__8AAIC5lwAVPJJzxnCAahSvZTIlG-HhtOvnM1uh-66x"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/JetBrainsMono-2.304.tar.gz")
                            (sha256 (base32 "1i2w213919avi0apgbw720wqy0z46a89bwv3b65hkbc2icg6jyn5")))))
                        ("N-V-__8AAL6FAwBDPampKgDjoxlJYDIn2jv0VaINS4W6CXJN"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/ghostty-themes-release-20260323-152405-a2c7b60.tgz")
                            (sha256 (base32 "0w26xk550adkijaf35r75v5k2gl7pm5wq4d1cr6zgnz18msifs3x")))))
                        ("N-V-__8AAEbOfQBnvcFcCX2W5z7tDaN8vaNZGamEQtNOe0UI"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://github.com/ocornut/imgui/archive/refs/tags/v1.92.5-docking.tar.gz")
                            (sha256 (base32 "1jzr65gpx4mqfcdbnf2rm2kd20jmj9whwdb7x1df3wvmih7c45n8")))))
                        ("N-V-__8AAGmZhABbsPJLfbqrh6JTHsXhY6qCaLAQyx25e0XE"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/highway-66486a10623fa0d72fe91260f96c892e41aceb06.tar.gz")
                            (sha256 (base32 "04m21b46h6c4x099r9qb720ql9llpzz8yq3k94i8zq7l7s4zim47")))))
                        ("N-V-__8AAG02ugUcWec-Ndp-i7JTsJ0dgF8nnJRUInkGLG7G"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/harfbuzz-11.0.0.tar.xz")
                            (sha256 (base32 "16rb7aazy36pj3xrjy149dd90j9yv7q5jnqx5kz2air1zsx52qzi")))))
                        ("N-V-__8AALiNBAA-_0gprYr92CjrMj1I5bqNu0TSJOnjFNSr"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/gtk4-layer-shell-1.1.0.tar.gz")
                            (sha256 (base32 "12396gx723ybgq1xp9i02257hsmzqhb5z9b39xdyypha4s0l4a4q")))))
                        ("gobject-0.3.0-Skun7ANLnwDvEfIpVmohcppXgOvg_I6YOJFmPIsKfXk-"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/gobject-2025-11-08-23-1.tar.zst")
                            (sha256 (base32 "0j0csvsyvp0193mpkdp25s14kargppmdyslbhi5qw788y0347gfr")))))
                        ("N-V-__8AABzkUgISeKGgXAzgtutgJsZc0-kkeqBBscJgMkvy"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/glslang-12201278a1a05c0ce0b6eb6026c65cd3e9247aa041b1c260324bf29cee559dd23ba1.tar.gz")
                            (sha256 (base32 "1dcpm70fhxk07vk37f5l0hb9gxfv6pjgbqskk8dfbcwwa2xyv8hl")))))
                        ("N-V-__8AADcZkgn4cMhTUpIz6mShCKyqqB-NBtf_S2bHaTC-"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/gettext-0.24.tar.gz")
                            (sha256 (base32 "1dqq2ln01mfwr4gblvy0cyvarbqnv09ml5sdhksdlw1xb4ym0669")))))
                        ("N-V-__8AAKLKpwC4H27Ps_0iL3bPkQb-z6ZVSrB-x_3EEkub"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/freetype-1220b81f6ecfb3fd222f76cf9106fecfa6554ab07ec7fdc4124b9bb063ae2adf969d.tar.gz")
                            (sha256 (base32 "035r5bypzapa1x7za7lpvpkz58fxynz4anqzbk8705hmspsh2wj2")))))
                        ("N-V-__8AAIrfdwARSa-zMmxWwFuwpXf1T3asIN7s5jqi9c1v"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/fontconfig-2.14.2.tar.gz")
                            (sha256 (base32 "0mcarq6v9k7k9a8is23vq9as0niv0hbagwdabknaq6472n9dv8iv")))))
                        ("N-V-__8AALw2uwF_03u4JRkZwRLc3Y9hakkYV7NKRR9-RIZJ"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/breakpad-b99f444ba5f6b98cac261cbb391d8766b34a5918.tar.gz")
                            (sha256 (base32 "1nbadlml3r982bz1wyp17w33hngzkb07f47nrrk0g68s7na9ijkc")))))
                        ("N-V-__8AANT61wB--nJ95Gj_ctmzAtcjloZ__hRqNw5lC1Kr"
                         (ungexp
                          (origin
                            (method url-fetch)
                            (uri "https://deps.files.ghostty.org/DearBindings_v0.17_ImGui_v1.92.5-docking.tar.gz")
                            (sha256 (base32 "18xsf0zisr9q8xqa8g3kh4qkqb9rrikkvqiyhmwzc9h9w00cbzlb")))))))))))))))
