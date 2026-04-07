(define-module (r0man guix packages speech)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public python-pywhispercpp
  (package
    (name "python-pywhispercpp")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/absadiki/pywhispercpp")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d0ks1l8vq4s59s2d6svnqkx4bz4pl8plyngv02hw9g9h3lliy7h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-no-repair
            (lambda _
              (setenv "NO_REPAIR" "1")))
          (add-after 'install 'fix-runpath
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (ice-9 popen)
                           (ice-9 textual-ports)
                           (srfi srfi-1))
              (let* ((out (assoc-ref outputs "out"))
                     (site-packages
                      (string-append out "/lib/python3.11/site-packages")))
                (for-each
                 (lambda (file)
                   (unless (symbolic-link? file)
                     (let* ((pipe (open-pipe* OPEN_READ "patchelf"
                                              "--print-rpath" file))
                            (rpath (string-trim-right
                                    (get-string-all pipe)))
                            (_ (close-pipe pipe))
                            (clean-paths
                             (filter
                              (lambda (p)
                                (and (not (string-null? p))
                                     (not (string-prefix? "/tmp/" p))))
                              (if (string-null? rpath)
                                  '()
                                  (string-split rpath #\:))))
                            (new-rpath
                             (string-join
                              (delete-duplicates
                               (cons site-packages clean-paths))
                              ":")))
                       (invoke "patchelf" "--set-rpath" new-rpath file)
                       (format #t "Fixed RUNPATH of ~a~%" file))))
                 (find-files site-packages "\\.so")))))
          (delete 'sanity-check))))
    (native-inputs
     (list cmake-minimal
           ninja
           patchelf
           pybind11
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy
           python-platformdirs
           python-requests
           python-tqdm))
    (home-page "https://github.com/absadiki/pywhispercpp")
    (synopsis "Python bindings for whisper.cpp")
    (description
     "pywhispercpp provides Python bindings for the whisper.cpp speech
recognition library.  It allows using whisper.cpp models for speech-to-text
transcription directly from Python.")
    (license license:expat)))

(define-public hyprwhspr
  (package
    (name "hyprwhspr")
    (version "1.25.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/goodroot/hyprwhspr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j8r9kym8r4rxx0j2qm4gv4h9l5fpxs5ahrvqbji5f6qv88fqs04"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("lib/" "lib/hyprwhspr/")
          ("bin/" "share/hyprwhspr/bin/")
          ("config/" "share/doc/hyprwhspr/config/")
          ("share/" "share/hyprwhspr/data/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patch-launcher
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (python (search-input-file inputs "/bin/python3"))
                     (lib-dir (string-append out "/lib/hyprwhspr"))
                     (launcher (string-append out "/bin/hyprwhspr")))
                (mkdir-p (string-append out "/bin"))
                (call-with-output-file launcher
                  (lambda (port)
                    (format port "#!~a~%" (search-input-file inputs "/bin/bash"))
                    (format port "export HYPRWHSPR_ROOT=~a~%" out)
                    (format port "export PYTHONPATH=~a:${PYTHONPATH}~%" lib-dir)
                    (format port "LIB_DIR=~a~%" lib-dir)
                    (format port "PYTHON_CMD=~a~%" python)
                    (format port "CLI_PYTHON=~a~%" python)
                    (newline port)
                    (format port "for arg in \"$@\"; do~%")
                    (format port "  if [[ \"$arg\" == \"-h\" || \
\"$arg\" == \"--help\" ]]; then~%")
                    (format port "    exec \"$CLI_PYTHON\" \"$LIB_DIR/cli.py\" --help~%")
                    (format port "  fi~%")
                    (format port "done~%")
                    (newline port)
                    (format port "if [[ \"$1\" == \"help\" ]]; then~%")
                    (format port "  exec \"$CLI_PYTHON\" \"$LIB_DIR/cli.py\" --help~%")
                    (format port "fi~%")
                    (newline port)
                    (format port "if [[ \"$1\" == \"test\" ]]; then~%")
                    (format port "  exec \"$PYTHON_CMD\" \"$LIB_DIR/cli.py\" \"$@\"~%")
                    (format port "fi~%")
                    (format port "case \"$1\" in~%")
                    (format port "  setup|install|config|waybar|~%")
                    (format port "  systemd|status|model|validate|~%")
                    (format port "  uninstall|backend|state|~%")
                    (format port "  mic-osd|keyboard|record)~%")
                    (format port "    exec \"$CLI_PYTHON\" \\~%")
                    (format port "      \"$LIB_DIR/cli.py\" \"$@\";;~%")
                    (format port "esac~%")
                    (newline port)
                    (format port "exec \"$PYTHON_CMD\" \"$LIB_DIR/main.py\" \"$@\"~%")))
                (chmod launcher #o755))))
          (add-after 'patch-launcher 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (launcher (string-append out "/bin/hyprwhspr"))
                     (python-path
                      (search-path-as-list
                       '("lib/python3.11/site-packages")
                       (map cdr inputs)))
                     (gi-typelib-path
                      (string-append
                       (assoc-ref inputs "gtk")
                       "/lib/girepository-1.0:"
                       (assoc-ref inputs "gtk4-layer-shell")
                       "/lib/girepository-1.0")))
                (wrap-program launcher
                  `("GUIX_PYTHONPATH" ":" prefix
                    (,(string-append out "/lib/hyprwhspr")
                     ,@python-path))
                  `("PATH" ":" prefix
                    (,(string-append (assoc-ref inputs "ydotool") "/bin")
                     ,(string-append (assoc-ref inputs "wtype") "/bin")
                     ,(string-append (assoc-ref inputs "wl-clipboard") "/bin")))
                  `("GI_TYPELIB_PATH" ":" prefix
                    (,gi-typelib-path)))))))))
    (inputs
     (list bash-minimal
           gtk
           gtk4-layer-shell
           python-wrapper
           python-dbus
           python-evdev
           python-numpy
           python-psutil
           python-pulsectl
           python-pygobject
           python-pyperclip
           python-pyudev
           python-pywhispercpp
           python-requests
           python-rich
           python-scipy
           python-sounddevice
           python-websocket-client
           wl-clipboard
           wtype
           ydotool))
    (home-page "https://github.com/goodroot/hyprwhspr")
    (synopsis "System-wide speech-to-text for Wayland")
    (description
     "hyprwhspr provides system-wide speech-to-text functionality for Wayland
compositors.  It uses whisper.cpp for local speech recognition and integrates
with Wayland tools for text input across applications.")
    (license license:expat)))
