(define-module (r0man packages python)
  #:use-module (gnu packages gcc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public pyhton-auditwheel
  (package
    (name "python-auditwheel")
    (version "5.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "auditwheel" version))
              (sha256
               (base32
                "0cx3np8g3py9bfjhng1s8zsb0k2pj8rgljpay6n7wxpvnssqggnh"))))
    (build-system python-build-system)
    (propagated-inputs (list python-importlib-metadata python-pyelftools))
    (native-inputs (list python-docker
                         python-jsonschema
                         python-pretend
                         python-pypatchelf
                         python-pytest
                         python-pytest-cov))
    (home-page "https://github.com/pypa/auditwheel")
    (synopsis "Cross-distribution Linux wheels")
    (description "Cross-distribution Linux wheels")
    (license license:expat)))

(define-public python-black
  (package
    (name "python-black")
    (version "22.12.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "black" version))
              (sha256
               (base32
                "0bvab7biqi0a9jnvc2q7mw9pxychz03x097pgd20z8wcl7jm34r2"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-click
                             python-mypy-extensions
                             python-pathspec
                             python-platformdirs
                             python-tomli
                             python-typed-ast
                             python-typing-extensions))
    (home-page "")
    (synopsis "The uncompromising code formatter.")
    (description "The uncompromising code formatter.")
    (license license:expat)))

(define-public python-hatchling
  (package
    (name "python-hatchling")
    (version "1.12.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hatchling" version))
              (sha256
               (base32
                "04dl87a3f8qx801v4fffnvx37dvg8kswidzjq6qr0rypsh1wjdjv"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-editables
                             python-importlib-metadata
                             python-packaging
                             python-pathspec
                             python-pluggy
                             python-tomli))
    (home-page "")
    (synopsis "Modern, extensible Python build backend")
    (description "Modern, extensible Python build backend")
    (license #f)))

(define-public python-pypatchelf
  (package
    (name "python-pypatchelf")
    (version "0.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pypatchelf" version))
              (sha256
               (base32
                "05dr7z42r3bdka62kha9cn3mxs81z409bd89w815ca2pvvqipfry"))))
    (build-system python-build-system)
    (home-page "https://github.com/nixos/patchelf")
    (synopsis
     "A small utility to modify the dynamic linker and RPATH of ELF executables")
    (description
     "This package provides a small utility to modify the dynamic linker and RPATH of
ELF executables")
    (license license:gpl3)))

(define-public python-wheel-0.37.1
  (package
    (inherit python-wheel)
    (name "python-wheel")
    (version "0.37.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wheel" version))
       (sha256
        (base32
         "1i6156ax8faybavfijdqnnks1x4w4jd9gjz9w2wa3g7gjgkh99g9"))))))

(define-public python-chatgpt-wrapper
  (package
    (name "python-chatgpt-wrapper")
    (version "39a1431a7adb9c8dc8d4ecffe57baa282c01269e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mmabrouk/chatgpt-wrapper.git")
             (commit (string-append "" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x9bj2yfnp3fsicz708ca66yl6gg8kv898z64h5sxirwkanlajq9"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; (delete 'sanity-check)
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              ;; Some tests fail when HOME=/homeless-shelter.
              (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list python-flask python-rich))
    (home-page "https://github.com/mmabrouk/chatgpt-wrapper")
    (synopsis "ChatGPT CLI and Python Wrapper")
    (description "API for interacting with ChatGPT using Python and from Shell.")
    (license #f)))

(define-public python-playwright
  (package
    (name "python-playwright")
    (version "1.29.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microsoft/playwright-python.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "066xmxc76jlqnjv7q924f9ss7ymnjm8q2hdqqznqm1801892a6d0"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; (delete 'sanity-check)
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              ;; Some tests fail when HOME=/homeless-shelter.
              (mkdir-p "/tmp/homeless-shelter")
              (setenv "HOME" "/tmp/homeless-shelter"))))))
    (propagated-inputs (list node
                             python-wheel-0.37.1
                             pyhton-auditwheel
                             python-autobahn
                             python-black
                             python-flake8
                             ))
    ;; (arguments
    ;;  (list
    ;;   #:phases
    ;;   #~(modify-phases %standard-phases
    ;;       ;; (delete 'sanity-check)
    ;;       (replace 'check
    ;;         (lambda* (#:key tests? #:allow-other-keys)
    ;;           ;; Some tests fail when HOME=/homeless-shelter.
    ;;           (setenv "HOME" "/tmp"))))))
    ;; (propagated-inputs (list python-flask python-rich))
    (home-page "https://github.com/Microsoft/playwright-python")
    (synopsis "Python version of the Playwright testing and automation library.")
    (description "Playwright is a Python library to automate Chromium, Firefox and
WebKit browsers with a single API. Playwright delivers automation that
is ever-green, capable, reliable and fast.")
    (license #f)))
