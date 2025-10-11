(define-module (r0man guix packages node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system node)
  #:use-module (guix gexp)
  #:use-module (gnu packages node-xyz))

(define-public node-get-port
  (package
    (name "node-get-port")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/get-port/-/get-port-"
         version ".tgz"))
       (sha256
        (base32
         "0pcrva2pyl778xlabwj0ghm00pafbl4qscxmfwnz4ychxsjx5igs"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/get-port")
    (synopsis "Get an available TCP port")
    (description
     "This package provides a Node.js module to get an available TCP port.")
    (license license:expat)))

(define-public node-is-url
  (package
    (name "node-is-url")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-url/-/is-url-"
         version ".tgz"))
       (sha256
        (base32
         "1ycq4fzpfjf645jqmkb9ps97xxslyilh339wyffjkpl2yks3ap20"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/segmentio/is-url")
    (synopsis "Check whether a string is a URL")
    (description
     "This package provides a simple utility to check if a string is a valid URL.")
    (license license:expat)))

(define-public node-upath
  (package
    (name "node-upath")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/upath/-/upath-"
         version ".tgz"))
       (sha256
        (base32
         "040xl5lszqz6akkszybz4rnmm5s3x3fc59mcs8fvyxy6wzp9rvbk"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/anodynos/upath")
    (synopsis "Unix-style path utilities for Node.js")
    (description
     "This package provides a proxy to Node.js path module, replacing backslashes
with forward slashes for all results and providing additional methods to
normalize and join paths while keeping leading ./ and manipulating file
extensions.")
    (license license:expat)))

(define-public node-lodash-isplainobject
  (package
    (name "node-lodash-isplainobject")
    (version "4.0.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/lodash.isplainobject/-/lodash.isplainobject-"
         version ".tgz"))
       (sha256
        (base32
         "1l3jmn7j4nzmv7pq24nawmjarb1gj647p786hjpjg5qkrvhj0r4q"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://lodash.com/")
    (synopsis "Lodash isPlainObject method as a module")
    (description
     "This package provides the lodash isPlainObject method exported as a Node.js
module.  It checks if a value is a plain object (an object created by the
Object constructor or one with a null prototype).")
    (license license:expat)))

(define-public node-argparse
  (package
    (name "node-argparse")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/argparse/-/argparse-"
         version ".tgz"))
       (sha256
        (base32
         "133jjyhcr25rf4vy7bca7x06dfmsyy819s1kbbyfc5c2zi3ki417"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/nodeca/argparse")
    (synopsis "CLI arguments parser for Node.js")
    (description
     "This package provides a CLI arguments parser for Node.js, which is a native
port of Python's argparse module.")
    (license license:expat)))

(define-public node-js-yaml
  (package
    (name "node-js-yaml")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/js-yaml/-/js-yaml-"
         version ".tgz"))
       (sha256
        (base32
         "1jpj5j4aiyh9sbcw7y8jjkwkyc6qmwrffw7a4qfb48ngb4jk7bhd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-argparse))
    (home-page "https://github.com/nodeca/js-yaml")
    (synopsis "YAML 1.2 parser and serializer for JavaScript")
    (description
     "This package provides a YAML parser and serializer for JavaScript.  It
supports YAML 1.2 specification and can be used in both Node.js and browsers.")
    (license license:expat)))

(define-public node-is-wsl
  (package
    (name "node-is-wsl")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-wsl/-/is-wsl-"
         version ".tgz"))
       (sha256
        (base32
         "0f3lcm9c4a00q7jp1ic40mixbqm9zqn9qbx50b4bsgrrgxxcqycc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-is-docker))
    (home-page "https://github.com/sindresorhus/is-wsl")
    (synopsis "Check if the process is running in Windows Subsystem for Linux")
    (description
     "This package provides a utility to detect if the current process is
running inside Windows Subsystem for Linux (WSL).")
    (license license:expat)))

(define-public node-is-docker
  (package
    (name "node-is-docker")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-docker/-/is-docker-"
         version ".tgz"))
       (sha256
        (base32
         "1b21xcvcb6qy2bj3m1p10swciqbl64irmfm6xghbpf1gyh0d05j8"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/is-docker")
    (synopsis "Check if the process is running inside a Docker container")
    (description
     "This package provides a utility to detect if the current process is
running inside a Docker container.")
    (license license:expat)))

(define-public node-define-lazy-prop
  (package
    (name "node-define-lazy-prop")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/define-lazy-prop/-/define-lazy-prop-"
         version ".tgz"))
       (sha256
        (base32
         "1rnfvih11v8lpmdykfgy461cray5vcf26xq8k81dxpph26rv2gfm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/define-lazy-prop")
    (synopsis "Define a lazily evaluated property on an object")
    (description
     "This package allows defining a property on an object that is lazily
evaluated on first access.")
    (license license:expat)))

(define-public node-open
  (package
    (name "node-open")
    (version "8.4.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/open/-/open-"
         version ".tgz"))
       (sha256
        (base32
         "1bd5xj5bkwn70xvkcvb4075l3ppzzjmzf8qybvsycv8l23alnynx"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-is-wsl node-is-docker node-define-lazy-prop))
    (home-page "https://github.com/sindresorhus/open")
    (synopsis "Open URLs, files, and executables")
    (description
     "This package provides a cross-platform way to open URLs, files, and
executables in their default application.")
    (license license:expat)))

(define-public node-ansi-regex
  (package
    (name "node-ansi-regex")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/ansi-regex/-/ansi-regex-"
         version ".tgz"))
       (sha256
        (base32
         "1ng0r2k4mcz7b2bfr6g1dschnxm0vifaslsvv2smv06smb6ss3hf"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/chalk/ansi-regex")
    (synopsis "Regular expression for matching ANSI escape codes")
    (description
     "This package provides a regular expression for matching ANSI escape codes.")
    (license license:expat)))

(define-public node-base64-js
  (package
    (name "node-base64-js")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/base64-js/-/base64-js-"
         version ".tgz"))
       (sha256
        (base32
         "118a46skxnrgx5bdd68ny9xxjcvyb7b1clj2hf82d196nm2skdxi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/beatgammit/base64-js")
    (synopsis "Base64 encoding/decoding in pure JavaScript")
    (description
     "This package provides base64 encoding and decoding functions in pure
JavaScript.  It supports both Node.js and browsers.")
    (license license:expat)))

(define-public node-clone
  (package
    (name "node-clone")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/clone/-/clone-"
         version ".tgz"))
       (sha256
        (base32
         "08w0b18a2bjh19fm2jwxyl7irxk8g4m0bm3ah99x7ph04iaxb4rk"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/pvorb/node-clone")
    (synopsis "Deep cloning of objects and arrays")
    (description
     "This package provides a deep cloning function for objects and arrays.")
    (license license:expat)))

(define-public node-signal-exit
  (package
    (name "node-signal-exit")
    (version "3.0.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/signal-exit/-/signal-exit-"
         version ".tgz"))
       (sha256
        (base32
         "1a10ixkiak24yy6s7p9m7c6v9jkz2fm7wxgc2l3614dbdbx275j3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/tapjs/signal-exit")
    (synopsis "Fire a callback when a process exits")
    (description
     "This package provides a way to register callbacks to be invoked when a
process is exiting.")
    (license license:isc)))

(define-public node-mimic-fn
  (package
    (name "node-mimic-fn")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/mimic-fn/-/mimic-fn-"
         version ".tgz"))
       (sha256
        (base32
         "1gv60if81lf2gkwvgixgsx8p87ddhsf1aswkihmfzi462hk5qw7a"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/mimic-fn")
    (synopsis "Make a function mimic another one")
    (description
     "This package provides a utility to make a function mimic another one by
copying properties like name, displayName, and other non-enumerable properties.")
    (license license:expat)))

(define-public node-has-flag
  (package
    (name "node-has-flag")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/has-flag/-/has-flag-"
         version ".tgz"))
       (sha256
        (base32
         "1cdmvliwz8h02nwg0ipli0ydd1l82sz9s1m7bj5bn9yr24afp9vp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/has-flag")
    (synopsis "Check if argv has a specific flag")
    (description
     "This package provides a utility to check if a specific flag is present
in the argv array.")
    (license license:expat)))

(define-public node-cli-spinners
  (package
    (name "node-cli-spinners")
    (version "2.9.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/cli-spinners/-/cli-spinners-"
         version ".tgz"))
       (sha256
        (base32
         "135l0bd57jgzjnkf3frpj32ya2rzynr95fz9qsbi4yxwvr74rhj6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/cli-spinners")
    (synopsis "Collection of CLI spinners")
    (description
     "This package provides a collection of spinners for use in command-line
interfaces.")
    (license license:expat)))

(define-public node-is-interactive
  (package
    (name "node-is-interactive")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-interactive/-/is-interactive-"
         version ".tgz"))
       (sha256
        (base32
         "194g35nginih951q6i1wvfk3wpff3kdrxn5hpzrjwjjmv1wqk11c"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/is-interactive")
    (synopsis "Check if stdout is interactive")
    (description
     "This package provides a utility to check if stdout or stderr is
interactive (i.e., connected to a terminal).")
    (license license:expat)))

(define-public node-is-unicode-supported
  (package
    (name "node-is-unicode-supported")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-unicode-supported/-/is-unicode-supported-"
         version ".tgz"))
       (sha256
        (base32
         "1gnakv580clcgcmh0nqrwqy53sdnrv4vglgixd2w8bic4bzgw5bw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/is-unicode-supported")
    (synopsis "Detect if the terminal supports Unicode")
    (description
     "This package provides a utility to detect if the terminal supports
Unicode characters.")
    (license license:expat)))

(define-public node-buffer
  (package
    (name "node-buffer")
    (version "5.7.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/buffer/-/buffer-"
         version ".tgz"))
       (sha256
        (base32
         "1g60az00dzb1grcszyg12gyrl9jr9bwvrk2y9xjdwym3nxasrgwq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-base64-js node-ieee754))
    (home-page "https://github.com/feross/buffer")
    (synopsis "Node.js Buffer API for the browser")
    (description
     "This package provides the Buffer API from Node.js for use in browsers
and other JavaScript environments.")
    (license license:expat)))

(define-public node-strip-ansi
  (package
    (name "node-strip-ansi")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/strip-ansi/-/strip-ansi-"
         version ".tgz"))
       (sha256
        (base32
         "1jh81jj6cn1lli1c7m6xi0ynra9zdghb1g63v1nib7zlpz87bnwv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-ansi-regex))
    (home-page "https://github.com/chalk/strip-ansi")
    (synopsis "Strip ANSI escape codes from strings")
    (description
     "This package provides a utility to strip ANSI escape codes from strings.")
    (license license:expat)))

(define-public node-color-convert
  (package
    (name "node-color-convert")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/color-convert/-/color-convert-"
         version ".tgz"))
       (sha256
        (base32
         "1qbw9rwfzcp7y0cpa8gmwlj7ccycf9pwn15zvf2s06f070ss83wj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-color-name))
    (home-page "https://github.com/Qix-/color-convert")
    (synopsis "Color conversion library for JavaScript")
    (description
     "This package provides color conversion utilities for JavaScript, supporting
conversions between various color models including RGB, HSL, HSV, and more.")
    (license license:expat)))

(define-public node-onetime
  (package
    (name "node-onetime")
    (version "5.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/onetime/-/onetime-"
         version ".tgz"))
       (sha256
        (base32
         "1kda4mbpk8csafchkakglpfyhsnmdkcl6gv1qi9v5dqwh3mb4ngh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-mimic-fn))
    (home-page "https://github.com/sindresorhus/onetime")
    (synopsis "Ensure a function is only called once")
    (description
     "This package provides a utility to ensure that a function is only called
once.  Subsequent calls will return the value from the first call.")
    (license license:expat)))

(define-public node-bl
  (package
    (name "node-bl")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/bl/-/bl-"
         version ".tgz"))
       (sha256
        (base32
         "1jx7lm4mr80nzdw0k873llpl1x6i1n0m422v1scwla8qml4vkpl3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-buffer node-inherits node-readable-stream))
    (home-page "https://github.com/rvagg/bl")
    (synopsis "Buffer List for Node.js")
    (description
     "This package provides a storage object for collections of Node.js Buffer
objects.  It provides a way to collect data from a stream into a list of
buffers.")
    (license license:expat)))

(define-public node-ansi-styles
  (package
    (name "node-ansi-styles")
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/ansi-styles/-/ansi-styles-"
         version ".tgz"))
       (sha256
        (base32
         "0zwqsx67hr7m4a8dpd0jzkp2rjm5v7938x4rhcqh7djsv139llrc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-color-convert))
    (home-page "https://github.com/chalk/ansi-styles")
    (synopsis "ANSI escape codes for styling strings in the terminal")
    (description
     "This package provides ANSI escape codes for styling strings in the
terminal.  It includes support for colors, background colors, and text styling.")
    (license license:expat)))

(define-public node-supports-color
  (package
    (name "node-supports-color")
    (version "7.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/supports-color/-/supports-color-"
         version ".tgz"))
       (sha256
        (base32
         "0jjyglzdzscmhgidn43zc218q5jf9h03hmaaq9h4wqil2vywlspi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-has-flag))
    (home-page "https://github.com/chalk/supports-color")
    (synopsis "Detect whether a terminal supports color")
    (description
     "This package provides a utility to detect whether a terminal supports
color output.")
    (license license:expat)))

(define-public node-defaults
  (package
    (name "node-defaults")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/defaults/-/defaults-"
         version ".tgz"))
       (sha256
        (base32
         "07l9fhw54y31467ijn780byl3ck72x32wmk2q2pvk6rpv28f1px6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-clone))
    (home-page "https://github.com/tmpvar/defaults")
    (synopsis "Merge objects with default values")
    (description
     "This package provides a utility to merge objects with default values.")
    (license license:expat)))

(define-public node-restore-cursor
  (package
    (name "node-restore-cursor")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/restore-cursor/-/restore-cursor-"
         version ".tgz"))
       (sha256
        (base32
         "0agcwdj60w84fji19wc2a6fj9b9apsf56yx3awlb0k0zndmp2j09"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-onetime node-signal-exit))
    (home-page "https://github.com/sindresorhus/restore-cursor")
    (synopsis "Restore the terminal cursor")
    (description
     "This package provides a utility to gracefully restore the terminal cursor
on exit.")
    (license license:expat)))

(define-public node-chalk
  (package
    (name "node-chalk")
    (version "4.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/chalk/-/chalk-"
         version ".tgz"))
       (sha256
        (base32
         "02prgl8d52k2vgxnssx06ha2sjm2vp6v6s6kqgkar1ryllx68k78"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-ansi-styles node-supports-color))
    (home-page "https://github.com/chalk/chalk")
    (synopsis "Terminal string styling for Node.js")
    (description
     "This package provides terminal string styling for Node.js.  It supports
256/Truecolor colors, bold, underline, italic, and strikethrough text styles.")
    (license license:expat)))

(define-public node-wcwidth
  (package
    (name "node-wcwidth")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/wcwidth/-/wcwidth-"
         version ".tgz"))
       (sha256
        (base32
         "1a53pbnxfmhnlvzzgq8p8yqh0gzlsibj7263gx8s86q63v1wna4z"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-defaults))
    (home-page "https://github.com/timoxley/wcwidth")
    (synopsis "Determine columns needed for a fixed-size wide-character string")
    (description
     "This package provides a port of wcwidth() and wcswidth() C functions to
JavaScript for determining the number of columns needed to display a string.")
    (license license:expat)))

(define-public node-cli-cursor
  (package
    (name "node-cli-cursor")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/cli-cursor/-/cli-cursor-"
         version ".tgz"))
       (sha256
        (base32
         "0k4qgv5xd9m6rh4aj20gzgdzf1zb647ddxpky3db911vqm6hxkxm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-restore-cursor))
    (home-page "https://github.com/sindresorhus/cli-cursor")
    (synopsis "Control cursor visibility in CLI applications")
    (description
     "This package provides utilities to show, hide, and toggle the terminal
cursor in command-line applications.")
    (license license:expat)))

(define-public node-log-symbols
  (package
    (name "node-log-symbols")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/log-symbols/-/log-symbols-"
         version ".tgz"))
       (sha256
        (base32
         "08qkf3cqwbi8azqxk2mz2mkfpndw71kklyk4x70a8aqvq6m0drrj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-chalk node-is-unicode-supported))
    (home-page "https://github.com/sindresorhus/log-symbols")
    (synopsis "Colored symbols for various log levels")
    (description
     "This package provides colored symbols for various log levels (info,
success, warning, error) for use in CLI applications.")
    (license license:expat)))

(define-public node-ora
  (package
    (name "node-ora")
    (version "5.4.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/ora/-/ora-"
         version ".tgz"))
       (sha256
        (base32
         "1657w05svjq62vlf0dvkhmbrh4hsbyash7504im8fi3apjhckv41"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-bl node-chalk node-cli-cursor node-cli-spinners
           node-is-interactive node-is-unicode-supported node-log-symbols
           node-strip-ansi node-wcwidth))
    (home-page "https://github.com/sindresorhus/ora")
    (synopsis "Elegant terminal spinner")
    (description
     "This package provides an elegant terminal spinner for Node.js.  It
supports over 80 spinner styles and allows customization of text, color, and
spinner type.")
    (license license:expat)))

(define-public node-depd
  (package
    (name "node-depd")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/depd/-/depd-"
         version ".tgz"))
       (sha256
        (base32
         "07645ghplj1qy8z6g3vz1855xjy2j217q90bib3m44c2npk6pql3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/dougwilson/nodejs-depd")
    (synopsis "Deprecation utility for Node.js")
    (description
     "This package provides a deprecation utility for Node.js.  It allows marking
properties and functions as deprecated and logs deprecation messages.")
    (license license:expat)))

(define-public node-vary
  (package
    (name "node-vary")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/vary/-/vary-"
         version ".tgz"))
       (sha256
        (base32
         "0wbf4kmfyzc23dc0vjcmymkd1ks50z5gvv23lkkkayipf438cy3k"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/vary")
    (synopsis "Manipulate the HTTP Vary header")
    (description
     "This package provides utilities to manipulate the HTTP Vary header for
proper cache handling.")
    (license license:expat)))

(define-public node-fresh
  (package
    (name "node-fresh")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/fresh/-/fresh-"
         version ".tgz"))
       (sha256
        (base32
         "0k44badcxkwy202kz404w078l660f65jaijg473xv38ay3wpdri5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/fresh")
    (synopsis "HTTP response freshness testing")
    (description
     "This package provides utilities to check if an HTTP response is fresh based
on the Last-Modified and/or ETag headers.")
    (license license:expat)))

(define-public node-cookie
  (package
    (name "node-cookie")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/cookie/-/cookie-"
         version ".tgz"))
       (sha256
        (base32
         "1z8xxh56qxgcz96j59aw7ik2847xk0lk91c9rdk38bkfbmncpy9f"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/cookie")
    (synopsis "HTTP cookie parsing and serialization")
    (description
     "This package provides basic HTTP cookie parsing and serialization for
Node.js and browsers.")
    (license license:expat)))

(define-public node-methods
  (package
    (name "node-methods")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/methods/-/methods-"
         version ".tgz"))
       (sha256
        (base32
         "0g50ci0gc8r8kq1i06q078gw7azkakp7j3yw5qfi6gq2qk8hdlnz"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/methods")
    (synopsis "HTTP request methods")
    (description
     "This package provides an array of all the standard HTTP request method
names.")
    (license license:expat)))

(define-public node-utils-merge
  (package
    (name "node-utils-merge")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/utils-merge/-/utils-merge-"
         version ".tgz"))
       (sha256
        (base32
         "0djhmrfzxpdhscg4pkgnsd39cddpwpkkw1w2f8mp2xfsxn7mvnfy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jaredhanson/utils-merge")
    (synopsis "Merge objects using descriptors")
    (description
     "This package provides utilities to merge objects using property descriptors.")
    (license license:expat)))

(define-public node-parseurl
  (package
    (name "node-parseurl")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/parseurl/-/parseurl-"
         version ".tgz"))
       (sha256
        (base32
         "06h2bx1rilkdir3v9jlg94r1q2fn895s0vxjjs0wx5z027x4pvsn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/pillarjs/parseurl")
    (synopsis "Parse a URL with memoization")
    (description
     "This package provides URL parsing with caching support for improved
performance.")
    (license license:expat)))

(define-public node-encodeurl
  (package
    (name "node-encodeurl")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/encodeurl/-/encodeurl-"
         version ".tgz"))
       (sha256
        (base32
         "13afvicx42ha4k29571sg0i4b76xjggyxvmmmibm258ipf6mjinb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/pillarjs/encodeurl")
    (synopsis "Encode a URL to a percent-encoded form")
    (description
     "This package provides a simple utility to encode URLs to percent-encoded
form, excluding already-encoded sequences.")
    (license license:expat)))

(define-public node-escape-html
  (package
    (name "node-escape-html")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/escape-html/-/escape-html-"
         version ".tgz"))
       (sha256
        (base32
         "0rh35dvab1wbp87dy1m6rynbcb9rbs5kry7jk17ixyxx7if1a0d1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/component/escape-html")
    (synopsis "Escape string for safe insertion into HTML")
    (description
     "This package provides a simple utility to escape HTML special characters
in strings.")
    (license license:expat)))

(define-public node-content-type
  (package
    (name "node-content-type")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/content-type/-/content-type-"
         version ".tgz"))
       (sha256
        (base32
         "0j0rkv7yvpdyk4hfgklx95g75aksljaxraz9lhkb5chhvxqkygnv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/content-type")
    (synopsis "Create and parse HTTP Content-Type header")
    (description
     "This package provides utilities to create and parse HTTP Content-Type
header according to RFC 7231.")
    (license license:expat)))

(define-public node-range-parser
  (package
    (name "node-range-parser")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/range-parser/-/range-parser-"
         version ".tgz"))
       (sha256
        (base32
         "09prs852snwqr9cfcrybm7ysl0z1wka9dh4dwc4v1415cvi6cllh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/range-parser")
    (synopsis "Parse Range header field in HTTP")
    (description
     "This package provides a utility to parse the Range header field in HTTP
requests.")
    (license license:expat)))

(define-public node-array-flatten
  (package
    (name "node-array-flatten")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/array-flatten/-/array-flatten-"
         version ".tgz"))
       (sha256
        (base32
         "1v96cj6w6f7g61c7fjfkxpkbbfkxl2ksh5zm7y5mfp96xivi5jhs"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/blakeembrey/array-flatten")
    (synopsis "Flatten nested arrays")
    (description
     "This package provides a utility to flatten nested arrays into a single-level
array.")
    (license license:expat)))

(define-public node-path-to-regexp
  (package
    (name "node-path-to-regexp")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/path-to-regexp/-/path-to-regexp-"
         version ".tgz"))
       (sha256
        (base32
         "0dlgr61ahgydnmsp1pprc1m52qnylkb3pdpvn1s5p5x8d7qn4m6y"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/component/path-to-regexp")
    (synopsis "Express-style path to regular expression")
    (description
     "This package provides utilities to convert path strings like
/user/:name into regular expressions.")
    (license license:expat)))

(define-public node-setprototypeof
  (package
    (name "node-setprototypeof")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/setprototypeof/-/setprototypeof-"
         version ".tgz"))
       (sha256
        (base32
         "13366ghwjzarwl9i537f1n6gkp85lggvngw81p6mpjy06hc52vx5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/wesleytodd/setprototypeof")
    (synopsis "Polyfill for Object.setPrototypeOf")
    (description
     "This package provides a polyfill for the Object.setPrototypeOf function.")
    (license license:isc)))

(define-public node-cookie-signature
  (package
    (name "node-cookie-signature")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/cookie-signature/-/cookie-signature-"
         version ".tgz"))
       (sha256
        (base32
         "04sk9ma5a8xb4jib4wmsdj8pz5bk36yzavzbj3k0drdy9bi4bww9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/tj/node-cookie-signature")
    (synopsis "Sign and unsign cookies")
    (description
     "This package provides utilities to sign and unsign cookies for tamper
detection.")
    (license license:expat)))

(define-public node-merge-descriptors
  (package
    (name "node-merge-descriptors")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/merge-descriptors/-/merge-descriptors-"
         version ".tgz"))
       (sha256
        (base32
         "02d4fqgiz4cc33vbcdlah9rafj5vc2z0iy05sc9wpfpzri3kn2l8"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/component/merge-descriptors")
    (synopsis "Merge objects using property descriptors")
    (description
     "This package provides a utility to merge objects using their property
descriptors.")
    (license license:expat)))

(define-public node-qs
  (package
    (name "node-qs")
    (version "6.7.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/qs/-/qs-"
         version ".tgz"))
       (sha256
        (base32
         "0kjlldn82gli5vwh4w84lv7xzlh5ayb91l90m08xsajins128bal"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/ljharb/qs")
    (synopsis "Querystring parser with nesting support")
    (description
     "This package provides a querystring parser with support for nesting, arrays,
and various encoding options.")
    (license license:bsd-3)))

(define-public node-etag
  (package
    (name "node-etag")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/etag/-/etag-"
         version ".tgz"))
       (sha256
        (base32
         "1bqgznlsrqcmxnhmnqkhwzcrqfaalxmfxzly1ikaplkkm5w6ragn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/etag")
    (synopsis "Create simple HTTP ETags")
    (description
     "This package provides utilities to create HTTP ETags for cache validation.")
    (license license:expat)))

(define-public node-statuses
  (package
    (name "node-statuses")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/statuses/-/statuses-"
         version ".tgz"))
       (sha256
        (base32
         "0g6ydb53k8b5rhll1z667riba9454ipkl4hgkc5vzc62l4h10g18"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/statuses")
    (synopsis "HTTP status utility")
    (description
     "This package provides utilities to work with HTTP status codes, including
converting between codes and messages.")
    (license license:expat)))

(define-public node-safe-buffer
  (package
    (name "node-safe-buffer")
    (version "5.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/safe-buffer/-/safe-buffer-"
         version ".tgz"))
       (sha256
        (base32
         "08ma0a2a9j537bxl7qd2dn6sjcdhrclpdbslr19bkbyc1z30d4p0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/feross/safe-buffer")
    (synopsis "Safer Node.js Buffer API")
    (description
     "This package provides a safer version of the Node.js Buffer API that works
across different Node.js versions.")
    (license license:expat)))

(define-public node-ms
  (package
    (name "node-ms")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/ms/-/ms-"
         version ".tgz"))
       (sha256
        (base32
         "1jrysw9zx14av3jdvc3kywc3xkjqxh748g4s6p1iy634i2mm489n"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/zeit/ms")
    (synopsis "Millisecond conversion utility")
    (description
     "This package provides a tiny utility to convert various time formats to
milliseconds.")
    (license license:expat)))

(define-public node-negotiator
  (package
    (name "node-negotiator")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/negotiator/-/negotiator-"
         version ".tgz"))
       (sha256
        (base32
         "1zpx97aamn044id45ljzalxpa9l1dm0gn260w5hwjg7998vxjz68"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/negotiator")
    (synopsis "HTTP content negotiation")
    (description
     "This package provides an HTTP content negotiator for Node.js.")
    (license license:expat)))

(define-public node-media-typer
  (package
    (name "node-media-typer")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/media-typer/-/media-typer-"
         version ".tgz"))
       (sha256
        (base32
         "07vlmddn91j0bbrxr2br320dnkxw96dp7hqmvidj5ydl84adiyid"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/media-typer")
    (synopsis "Simple RFC 6838 media type parser")
    (description
     "This package provides a simple parser for RFC 6838 media types.")
    (license license:expat)))

(define-public node-forwarded
  (package
    (name "node-forwarded")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/forwarded/-/forwarded-"
         version ".tgz"))
       (sha256
        (base32
         "1kajx1hlidman6sircpxj5hpxw6q06p2li7hq15skff7k00prraq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/forwarded")
    (synopsis "Parse HTTP X-Forwarded-For header")
    (description
     "This package provides utilities to parse the X-Forwarded-For header.")
    (license license:expat)))

(define-public node-ipaddr-js
  (package
    (name "node-ipaddr-js")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/ipaddr.js/-/ipaddr.js-"
         version ".tgz"))
       (sha256
        (base32
         "1pz2yil3brgncrpbfpmzrj3fhj3di7hqjxk7vm9hrjqs9fsd1vhh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/whitequark/ipaddr.js")
    (synopsis "IPv4 and IPv6 address manipulation library")
    (description
     "This package provides a library for manipulating IPv4 and IPv6 addresses
in JavaScript.")
    (license license:expat)))

(define-public node-bytes
  (package
    (name "node-bytes")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/bytes/-/bytes-"
         version ".tgz"))
       (sha256
        (base32
         "1w0cw2wnzif217yvnldrflj14l2zkfcxdw1yhnv1jbm6ywykmngp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/visionmedia/bytes.js")
    (synopsis "Byte string parser and formatter")
    (description
     "This package provides utilities to parse and format byte values.")
    (license license:expat)))

(define-public node-ee-first
  (package
    (name "node-ee-first")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/ee-first/-/ee-first-"
         version ".tgz"))
       (sha256
        (base32
         "175r500n567a04qmswzw5hkgdnika3dvn63n284jlar2gvmyhj2i"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jonathanong/ee-first")
    (synopsis "Return the first event in a set of event emitters")
    (description
     "This package provides utilities to get the first event from a set of
EventEmitters.")
    (license license:expat)))

(define-public node-unpipe
  (package
    (name "node-unpipe")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/unpipe/-/unpipe-"
         version ".tgz"))
       (sha256
        (base32
         "1dnzbqfmchls4jyvkw0wnkc09pig98y66zzsy3lizgyls435xyrd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/stream-utils/unpipe")
    (synopsis "Unpipe a stream from all destinations")
    (description
     "This package provides a utility to remove all pipe destinations from a
stream.")
    (license license:expat)))

(define-public node-mime
  (package
    (name "node-mime")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/mime/-/mime-"
         version ".tgz"))
       (sha256
        (base32
         "16iprk4h6nh780mvfv0p93k3yvj7jrq2qs92niaw6yk11qwi0li1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure)
          (add-after 'patch-dependencies 'remove-build-files
            (lambda _
              (delete-file-recursively "src")
              (substitute* "package.json"
                (("\"scripts\":[^}]*},") "")))))))
    (home-page "https://github.com/broofa/node-mime")
    (synopsis "Comprehensive MIME type mapping API")
    (description
     "This package provides a comprehensive MIME type mapping API based on
mime-db module.")
    (license license:expat)))

(define-public node-destroy
  (package
    (name "node-destroy")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/destroy/-/destroy-"
         version ".tgz"))
       (sha256
        (base32
         "0miy8a2wfc77l4j08hv4qk5v9a7566igql71zw7ds3v5yhi9cmsv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/stream-utils/destroy")
    (synopsis "Destroy a stream if possible")
    (description
     "This package provides utilities to destroy a stream if it has a destroy or
close method.")
    (license license:expat)))

(define-public node-mime-db
  (package
    (name "node-mime-db")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/mime-db/-/mime-db-"
         version ".tgz"))
       (sha256
        (base32
         "1ki5v1b41dfb171bdcs8i469aycfg2b4i6raig8vk64wyp19bif5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jshttp/mime-db")
    (synopsis "Media type database")
    (description
     "This package provides a comprehensive database of mime types from various
sources including IANA, Apache, and nginx.")
    (license license:expat)))

(define-public node-safer-buffer
  (package
    (name "node-safer-buffer")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/safer-buffer/-/safer-buffer-"
         version ".tgz"))
       (sha256
        (base32
         "1cx383s7vchfac8jlg3mnb820hkgcvhcpfn9w4f0g61vmrjjz0bq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/ChALkeR/safer-buffer")
    (synopsis "Safer Node.js Buffer API")
    (description
     "This package provides a modern drop-in replacement for Node.js Buffer
with additional safety checks.")
    (license license:expat)))

(define-public node-toidentifier
  (package
    (name "node-toidentifier")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/toidentifier/-/toidentifier-"
         version ".tgz"))
       (sha256
        (base32
         "1i9qgk5k664mfvc7zj80mz5af7py2vh0zmr8yrvbb0fkzj1d3vba"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/component/toidentifier")
    (synopsis "Convert string to identifier")
    (description
     "This package provides a utility to convert HTTP header field names to
ECMAScript identifiers.")
    (license license:expat)))

(define-public node-mime-types
  (package
    (name "node-mime-types")
    (version "2.1.24")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/mime-types/-/mime-types-"
         version ".tgz"))
       (sha256
        (base32
         "1n7fsi13xp213dxdcs0qfyk596nbsaklbfxnd4xjnhnf65abcsy1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-mime-db))
    (home-page "https://github.com/jshttp/mime-types")
    (synopsis "MIME type association library")
    (description
     "This package provides utilities to determine the content type of a file based on its extension.")
    (license license:expat)))

(define-public node-iconv-lite
  (package
    (name "node-iconv-lite")
    (version "0.4.24")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/iconv-lite/-/iconv-lite-"
         version ".tgz"))
       (sha256
        (base32
         "0da6ff7dlx6lfhdafsd9sv0h09sicpfakms8bqylrm4f17r68v2p"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-safer-buffer))
    (home-page "https://github.com/ashtuchkin/iconv-lite")
    (synopsis "Character encoding conversion library")
    (description
     "This package provides pure JavaScript character encoding conversion for Node.js.")
    (license license:expat)))

(define-public node-http-errors
  (package
    (name "node-http-errors")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/http-errors/-/http-errors-"
         version ".tgz"))
       (sha256
        (base32
         "10x0729bip3xz9p4qj8hbgp0hfbknnv8imnp6r964hn5aakd8xys"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-depd node-inherits node-statuses node-toidentifier node-setprototypeof))
    (home-page "https://github.com/jshttp/http-errors")
    (synopsis "Create HTTP errors")
    (description
     "This package provides utilities to create HTTP error objects for use in Express applications.")
    (license license:expat)))

(define-public node-debug
  (package
    (name "node-debug")
    (version "2.6.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/debug/-/debug-"
         version ".tgz"))
       (sha256
        (base32
         "160wvc74r8aypds7pym3hq4qpa786hpk4vif58ggiwcqcv34ibil"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-ms))
    (home-page "https://github.com/visionmedia/debug")
    (synopsis "Small debugging utility")
    (description
     "This package provides a tiny JavaScript debugging utility modeled after Node.js core debugging.")
    (license license:expat)))

(define-public node-accepts
  (package
    (name "node-accepts")
    (version "1.3.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/accepts/-/accepts-"
         version ".tgz"))
       (sha256
        (base32
         "1s550j2wkqhsgpj841fww4bdck0w67rk80qb859nwqy8x7khsycs"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-mime-types node-negotiator))
    (home-page "https://github.com/jshttp/accepts")
    (synopsis "Higher-level content negotiation")
    (description
     "This package provides higher-level content negotiation based on negotiator.")
    (license license:expat)))

(define-public node-type-is
  (package
    (name "node-type-is")
    (version "1.6.18")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/type-is/-/type-is-"
         version ".tgz"))
       (sha256
        (base32
         "1bn3gl9vd67cq3wl2cvq686zskl2xx6lxz5kp9w47qc06f2vbnll"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-mime-types node-media-typer))
    (home-page "https://github.com/jshttp/type-is")
    (synopsis "Infer the content-type of a request")
    (description
     "This package provides utilities to infer the content-type of a request.")
    (license license:expat)))

(define-public node-proxy-addr
  (package
    (name "node-proxy-addr")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/proxy-addr/-/proxy-addr-"
         version ".tgz"))
       (sha256
        (base32
         "1wddayhxs5332azjn4a20pvnhc1gajwissc4kmba0rv2dx8y15cd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-forwarded node-ipaddr-js))
    (home-page "https://github.com/jshttp/proxy-addr")
    (synopsis "Determine address of proxied request")
    (description
     "This package provides utilities to determine the address of a proxied request.")
    (license license:expat)))

(define-public node-on-finished
  (package
    (name "node-on-finished")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/on-finished/-/on-finished-"
         version ".tgz"))
       (sha256
        (base32
         "0sv1js3fk0ag46ln3a7cwhannacqxyl694zkb1qy53fdd630sr59"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-ee-first))
    (home-page "https://github.com/jshttp/on-finished")
    (synopsis "Execute callback when HTTP request closes")
    (description
     "This package provides utilities to execute a callback when an HTTP request closes, finishes, or errors.")
    (license license:expat)))

(define-public node-content-disposition
  (package
    (name "node-content-disposition")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/content-disposition/-/content-disposition-"
         version ".tgz"))
       (sha256
        (base32
         "1gnpp7mvy8r2k8a4kx43rhg8l85n2g0rfvyfn7wai2k42zk08q4y"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-safe-buffer))
    (home-page "https://github.com/jshttp/content-disposition")
    (synopsis "Create and parse Content-Disposition header")
    (description
     "This package provides utilities to create and parse the HTTP Content-Disposition header.")
    (license license:expat)))

(define-public node-raw-body
  (package
    (name "node-raw-body")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/raw-body/-/raw-body-"
         version ".tgz"))
       (sha256
        (base32
         "0l1v7r5mn6jk7f3h0imryzsz0cigkbq97nigdqvlna5q2v733qzh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-bytes node-unpipe node-iconv-lite node-http-errors))
    (home-page "https://github.com/stream-utils/raw-body")
    (synopsis "Get and validate raw body of a request")
    (description
     "This package provides utilities to get and validate the raw body of a readable stream.")
    (license license:expat)))

(define-public node-finalhandler
  (package
    (name "node-finalhandler")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/finalhandler/-/finalhandler-"
         version ".tgz"))
       (sha256
        (base32
         "1kq09av23a28ig4kd727librz6dmiwjycsr7nh5cg0vjy0bk31pj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-debug node-unpipe node-parseurl node-statuses
           node-encodeurl node-escape-html node-on-finished))
    (home-page "https://github.com/pillarjs/finalhandler")
    (synopsis "Final HTTP responder")
    (description
     "This package provides a function to invoke as the final step to respond to HTTP requests.")
    (license license:expat)))

(define-public node-send
  (package
    (name "node-send")
    (version "0.17.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/send/-/send-"
         version ".tgz"))
       (sha256
        (base32
         "08ds2hx476lhafl051sh03kaiilkariwa209d02gbcm6xygb8m6k"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-ms node-depd node-etag node-mime node-debug node-fresh
           node-destroy node-statuses node-encodeurl node-escape-html
           node-http-errors node-on-finished node-range-parser))
    (home-page "https://github.com/pillarjs/send")
    (synopsis "Streaming static file server")
    (description
     "This package provides utilities for streaming files from the file system as an HTTP response.")
    (license license:expat)))

(define-public node-serve-static
  (package
    (name "node-serve-static")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/serve-static/-/serve-static-"
         version ".tgz"))
       (sha256
        (base32
         "1pajpv2acavzq2bpj3rggrvik00k9wyav2fvg5yvmvgmwy3kbxh1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-send node-parseurl node-encodeurl node-escape-html))
    (home-page "https://github.com/expressjs/serve-static")
    (synopsis "Serve static files")
    (description
     "This package provides middleware for serving static files in Express applications.")
    (license license:expat)))

(define-public node-body-parser
  (package
    (name "node-body-parser")
    (version "1.19.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/body-parser/-/body-parser-"
         version ".tgz"))
       (sha256
        (base32
         "18vy9ymfhmp3sisf3nxvxi0rl2xll8jp3wd84imnj8swyny5jm02"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-qs node-depd node-bytes node-debug node-type-is node-raw-body
           node-iconv-lite node-http-errors node-on-finished node-content-type))
    (home-page "https://github.com/expressjs/body-parser")
    (synopsis "Node.js body parsing middleware")
    (description
     "This package provides middleware to parse incoming request bodies in Express
applications, making the data available under req.body property.")
    (license license:expat)))

(define-public node-express
  (package
    (name "node-express")
    (version "4.17.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/express/-/express-"
         version ".tgz"))
       (sha256
        (base32
         "1a82maaz62wcw1dsv863ikjp6gpyxka0b1g2sldnvxg3qi8va016"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-qs node-depd node-etag node-send node-vary node-debug node-fresh
           node-cookie node-accepts node-methods node-type-is node-parseurl node-statuses
           node-encodeurl node-proxy-addr node-body-parser node-escape-html node-on-finished
           node-safe-buffer node-utils-merge node-content-type node-finalhandler node-range-parser
           node-serve-static node-array-flatten node-path-to-regexp node-setprototypeof
           node-cookie-signature node-merge-descriptors node-content-disposition))
    (home-page "https://expressjs.com/")
    (synopsis "Fast, unopinionated, minimalist web framework for Node.js")
    (description
     "Express is a minimal and flexible Node.js web application framework that
provides a robust set of features for web and mobile applications.")
    (license license:expat)))

(define-public node-duplexer3
  (package
    (name "node-duplexer3")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/duplexer3/-/duplexer3-"
         version ".tgz"))
       (sha256
        (base32
         "1dryazypdc62snx1sja0fxg0a7pp55r28wwgiyai0lsyjfh4vmaw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/floatdrop/duplexer3")
    (synopsis "Like duplexer but using streams3")
    (description
     "This package provides a duplex stream that forwards reads and writes to another duplex stream.")
    (license license:bsd-3)))

(define-public node-type-fest
  (package
    (name "node-type-fest")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/type-fest/-/type-fest-"
         version ".tgz"))
       (sha256
        (base32
         "1gafiks4h2k1328k13mk9j00b9jigmgjsnckw7k7gakxvfk78c8b"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/type-fest")
    (synopsis "Collection of essential TypeScript types")
    (description
     "This package provides a collection of essential TypeScript types.")
    (license license:expat)))

(define-public node-lowercase-keys
  (package
    (name "node-lowercase-keys")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/lowercase-keys/-/lowercase-keys-"
         version ".tgz"))
       (sha256
        (base32
         "0l9vf2m7zz1hw8s0l2njhk3b128mq0z27z17dgpmk4ivcg181ij3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/lowercase-keys")
    (synopsis "Lowercase the keys of an object")
    (description
     "This package provides utilities to lowercase all keys in an object.")
    (license license:expat)))

(define-public node-mimic-response
  (package
    (name "node-mimic-response")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/mimic-response/-/mimic-response-"
         version ".tgz"))
       (sha256
        (base32
         "09scpfzi3b31b2imhd4g15q4asqzr1ph8la3q7cfb0cpdl7fsasy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/mimic-response")
    (synopsis "Mimic a Node.js HTTP response stream")
    (description
     "This package provides utilities to mimic a Node.js HTTP response stream.")
    (license license:expat)))

(define-public node-responselike
  (package
    (name "node-responselike")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/responselike/-/responselike-"
         version ".tgz"))
       (sha256
        (base32
         "1xaszdarkass77bwzijlnrqfsh05rkvgf2b6909hcf0pn31s8bvy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-lowercase-keys))
    (home-page "https://github.com/sindresorhus/responselike")
    (synopsis "Response-like object")
    (description
     "This package provides a response-like object for mocking HTTP responses.")
    (license license:expat)))

(define-public node-to-readable-stream
  (package
    (name "node-to-readable-stream")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/to-readable-stream/-/to-readable-stream-"
         version ".tgz"))
       (sha256
        (base32
         "1kgljs4zz58bfj5y943sgb55iniddljx6bixnimgwyg4jzgp9v6b"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/to-readable-stream")
    (synopsis "Convert a string/Buffer/Uint8Array to a readable stream")
    (description
     "This package provides utilities to convert strings and buffers to readable streams.")
    (license license:expat)))

(define-public node-json-buffer
  (package
    (name "node-json-buffer")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/json-buffer/-/json-buffer-"
         version ".tgz"))
       (sha256
        (base32
         "19gmzrfyy4k7sckqgc3gsz5hpkjg6gg75nhrr5rmyg3ip56ldxyi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/dominictarr/json-buffer")
    (synopsis "JSON parse and stringify using buffers")
    (description
     "This package provides JSON parsing and stringification that preserves Buffer objects.")
    (license license:expat)))

(define-public node-defer-to-connect
  (package
    (name "node-defer-to-connect")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/defer-to-connect/-/defer-to-connect-"
         version ".tgz"))
       (sha256
        (base32
         "0cnlka4hxw7mrl8rphpslcbxb5pc8d7w6jwv1fhpmj89nx6hfny3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure)
          (add-after 'patch-dependencies 'remove-build-script
            (lambda _
              (substitute* "package.json"
                (("\"scripts\":[^}]*},") "")))))))
    (home-page "https://github.com/szmarczak/defer-to-connect")
    (synopsis "Defer to connect for HTTP agents")
    (description
     "This package provides utilities to defer function calls to when the socket connects.")
    (license license:expat)))

(define-public node-end-of-stream
  (package
    (name "node-end-of-stream")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/end-of-stream/-/end-of-stream-"
         version ".tgz"))
       (sha256
        (base32
         "1d8dvwmq5krcjakr2s7b2dwh8x6zgs8qj8mdwkgxsvx7rgqfr2qb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-once))
    (home-page "https://github.com/mafintosh/end-of-stream")
    (synopsis "Call a callback when a stream has completed or failed")
    (description
     "This package provides a utility to handle stream completion and error events.")
    (license license:expat)))

(define-public node-pump
  (package
    (name "node-pump")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/pump/-/pump-"
         version ".tgz"))
       (sha256
        (base32
         "0zhaxbshjzgjnxaxl47i9rzkbgc664an29nsdr3m3ibdj9csxrmb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-end-of-stream
           node-once))
    (home-page "https://github.com/mafintosh/pump")
    (synopsis "Pipe streams together with error handling")
    (description
     "This package provides utilities to pipe streams together and close all of them if one fails.")
    (license license:expat)))

(define-public node-get-stream
  (package
    (name "node-get-stream")
    (version "5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/get-stream/-/get-stream-"
         version ".tgz"))
       (sha256
        (base32
         "077iv0jqfgqyxw6jb0iradw9idjcmphmrc58zj050vf58yx0bzkr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-pump))
    (home-page "https://github.com/sindresorhus/get-stream")
    (synopsis "Get a stream as a string, buffer, or array")
    (description
     "This package provides utilities to get the contents of a stream.")
    (license license:expat)))

(define-public node-sindresorhus-is
  (package
    (name "node-sindresorhus-is")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/@sindresorhus/is/-/is-"
         version ".tgz"))
       (sha256
        (base32
         "0m1y0gmbvaijc3fmrh2w5kzr36sgppp720h3ald72m2ri1sp0sk8"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/is")
    (synopsis "Type check values")
    (description
     "This package provides utilities for type checking values in JavaScript.")
    (license license:expat)))

(define-public node-cacheable-lookup
  (package
    (name "node-cacheable-lookup")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/cacheable-lookup/-/cacheable-lookup-"
         version ".tgz"))
       (sha256
        (base32
         "03lyfq5q15rqpnpgazk7nadxlvrd8mja4v3fnyv4a61kcpp5s1x1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-keyv
           node-types-keyv))
    (home-page "https://github.com/szmarczak/cacheable-lookup")
    (synopsis "Cacheable DNS lookups")
    (description
     "This package provides a cacheable dns.lookup() that respects TTL.")
    (license license:expat)))

(define-public node-decompress-response
  (package
    (name "node-decompress-response")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/decompress-response/-/decompress-response-"
         version ".tgz"))
       (sha256
        (base32
         "01pxda5wsdi8xm04dk0p9kgr3wa4dxx9ancqriq7bi8bm0fivq00"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-mimic-response))
    (home-page "https://github.com/sindresorhus/decompress-response")
    (synopsis "Decompress a HTTP response if needed")
    (description
     "This package provides utilities to decompress an HTTP response if it's compressed.")
    (license license:expat)))

(define-public node-szmarczak-http-timer
  (package
    (name "node-szmarczak-http-timer")
    (version "4.0.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/@szmarczak/http-timer/-/http-timer-"
         version ".tgz"))
       (sha256
        (base32
         "0mibapm04v1qka4n0645r5cyrg127cs3j5gdapmpn3c74plgi9zx"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure)
          (add-after 'patch-dependencies 'remove-build-script
            (lambda _
              (substitute* "package.json"
                (("\"scripts\":[^}]*},") "")))))))
    (inputs
     (list node-defer-to-connect))
    (home-page "https://github.com/szmarczak/http-timer")
    (synopsis "HTTP request timings")
    (description
     "This package provides timings for HTTP requests.")
    (license license:expat)))

(define-public node-undici-types
  (package
    (name "node-undici-types")
    (version "6.20.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/undici-types/-/undici-types-"
         version ".tgz"))
       (sha256
        (base32
         "0vldbr517hhwc3kpbxmwqag2inxvdzibaj6nvkj74qv7zzyak33j"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/nodejs/undici")
    (synopsis "TypeScript definitions for Undici")
    (description
     "This package provides TypeScript type definitions for the Undici HTTP client.")
    (license license:expat)))

(define-public node-types-http-cache-semantics
  (package
    (name "node-types-http-cache-semantics")
    (version "4.0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/@types/http-cache-semantics/-/http-cache-semantics-"
         version ".tgz"))
       (sha256
        (base32
         "1banb2jnydls22dlkg5disqhymkh5gxdlv5lvwgrharq0jisic6z"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/DefinitelyTyped/DefinitelyTyped")
    (synopsis "TypeScript definitions for http-cache-semantics")
    (description
     "This package provides TypeScript type definitions for http-cache-semantics.")
    (license license:expat)))

(define-public node-types-node
  (package
    (name "node-types-node")
    (version "22.10.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/@types/node/-/node-"
         version ".tgz"))
       (sha256
        (base32
         "0ikzsqx2zb363psjv7pjylg8iv4hma8mvgfyx7r57y20q316vrpi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-undici-types))
    (home-page "https://github.com/DefinitelyTyped/DefinitelyTyped")
    (synopsis "TypeScript definitions for Node.js")
    (description
     "This package provides TypeScript type definitions for Node.js.")
    (license license:expat)))

(define-public node-types-responselike
  (package
    (name "node-types-responselike")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/@types/responselike/-/responselike-"
         version ".tgz"))
       (sha256
        (base32
         "0lfhnawymczrpbn8p215hwl3lba2rmxbqfs4fmilnf5m5ifnz3nn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-types-node))
    (home-page "https://github.com/DefinitelyTyped/DefinitelyTyped")
    (synopsis "TypeScript definitions for responselike")
    (description
     "This package provides TypeScript type definitions for responselike.")
    (license license:expat)))

(define-public node-types-cacheable-request
  (package
    (name "node-types-cacheable-request")
    (version "6.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/@types/cacheable-request/-/cacheable-request-"
         version ".tgz"))
       (sha256
        (base32
         "174gfmvmy7plhy08ph9xwiiwknz5giawdaa5fp65snz7qm9a4d25"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-types-http-cache-semantics
           node-types-keyv
           node-types-node
           node-types-responselike))
    (home-page "https://github.com/DefinitelyTyped/DefinitelyTyped")
    (synopsis "TypeScript definitions for cacheable-request")
    (description
     "This package provides TypeScript type definitions for cacheable-request.")
    (license license:expat)))

(define-public node-p-cancelable
  (package
    (name "node-p-cancelable")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/p-cancelable/-/p-cancelable-"
         version ".tgz"))
       (sha256
        (base32
         "1l20j33lw75r4hvrlhahvaj46dwkj8xw45y180i5g3q0qwip1clh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/p-cancelable")
    (synopsis "Create a promise that can be canceled")
    (description
     "This package provides utilities to create cancelable promises.")
    (license license:expat)))

(define-public node-p-finally
  (package
    (name "node-p-finally")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/p-finally/-/p-finally-"
         version ".tgz"))
       (sha256
        (base32
         "0hc8zfqhw1qnrbm62dxqb7a019r1vbsg2hx2hhm58c0kkvh89zgg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/p-finally")
    (synopsis "Promise#finally() ponyfill")
    (description
     "This package provides a ponyfill for Promise.prototype.finally().")
    (license license:expat)))

(define-public node-keyv
  (package
    (name "node-keyv")
    (version "4.5.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/keyv/-/keyv-"
         version ".tgz"))
       (sha256
        (base32
         "1630gds0l00zmx2kiaa76ybf6ig7ygsm8ssy6sjg8yp5c72z29pb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure)
          (add-after 'patch-dependencies 'remove-build-script
            (lambda _
              (substitute* "package.json"
                (("\"scripts\":[^}]*},") "")))))))
    (inputs
     (list node-json-buffer))
    (home-page "https://github.com/jaredwray/keyv")
    (synopsis "Simple key-value storage with support for multiple backends")
    (description
     "This package provides a simple key-value storage API with support for multiple backends.")
    (license license:expat)))

(define-public node-types-keyv
  (package
    (name "node-types-keyv")
    (version "3.1.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/@types/keyv/-/keyv-"
         version ".tgz"))
       (sha256
        (base32
         "0lindx0xh0p57z20y7iylidl3w00dksr02q393h186iva957j732"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-types-node))
    (home-page "https://github.com/DefinitelyTyped/DefinitelyTyped")
    (synopsis "TypeScript definitions for keyv")
    (description
     "This package provides TypeScript type definitions for keyv.")
    (license license:expat)))

(define-public node-normalize-url
  (package
    (name "node-normalize-url")
    (version "6.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/normalize-url/-/normalize-url-"
         version ".tgz"))
       (sha256
        (base32
         "03iv7s36r9alrm2krzs05ansrbivd8zgww7f12abdy5sd78b8d4g"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/normalize-url")
    (synopsis "Normalize a URL")
    (description
     "This package provides utilities to normalize URLs into a consistent format.")
    (license license:expat)))

(define-public node-http-cache-semantics
  (package
    (name "node-http-cache-semantics")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/http-cache-semantics/-/http-cache-semantics-"
         version ".tgz"))
       (sha256
        (base32
         "0lrjgcds15b4m50qkkzs16slwnn7s3rmbqg6q14r2j0i9bhrigmr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/kornelski/http-cache-semantics")
    (synopsis "HTTP caching semantics")
    (description
     "This package implements RFC 7234 HTTP caching semantics.")
    (license license:bsd-2)))

(define-public node-mimic-response-1
  (package
    (name "node-mimic-response-1")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/mimic-response/-/mimic-response-"
         version ".tgz"))
       (sha256
        (base32
         "1s4cran004criblxk656ajagx924cy3wps608ksssyxc1qn01wa2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/mimic-response")
    (synopsis "Mimic a Node.js HTTP response stream (v1)")
    (description
     "This package provides utilities to mimic Node.js HTTP response streams. This is version 1.x for compatibility.")
    (license license:expat)))

(define-public node-clone-response
  (package
    (name "node-clone-response")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/clone-response/-/clone-response-"
         version ".tgz"))
       (sha256
        (base32
         "0rp9mi1n1dfya1ibq3r17z6g9iqx3q7nz9gphslkwj6vpf14kf1i"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-mimic-response-1))
    (home-page "https://github.com/lukechilds/clone-response")
    (synopsis "Clone a Node.js response stream")
    (description
     "This package provides utilities to clone Node.js HTTP response streams.")
    (license license:expat)))

(define-public node-p-timeout
  (package
    (name "node-p-timeout")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/p-timeout/-/p-timeout-"
         version ".tgz"))
       (sha256
        (base32
         "0rqwv9w1cd6ma2dp31z2l9xckx62brngkxg3062r4dx8w230hsr2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-p-finally))
    (home-page "https://github.com/sindresorhus/p-timeout")
    (synopsis "Timeout a promise after a specified amount of time")
    (description
     "This package provides utilities to add timeout functionality to promises.")
    (license license:expat)))

(define-public node-p-event
  (package
    (name "node-p-event")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/p-event/-/p-event-"
         version ".tgz"))
       (sha256
        (base32
         "1y4ag4xa0b2xk9m9hkm8g139089lfdxgx9m82r47273pcad0jwx5"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-p-timeout))
    (home-page "https://github.com/sindresorhus/p-event")
    (synopsis "Promisify an event by waiting for it to be emitted")
    (description
     "This package provides utilities to convert EventEmitter events into promises.")
    (license license:expat)))

(define-public node-cacheable-request
  (package
    (name "node-cacheable-request")
    (version "7.0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/cacheable-request/-/cacheable-request-"
         version ".tgz"))
       (sha256
        (base32
         "13l93qyb31yma8dg719xfk6rl8wjwpchkz2wvng95y4gap5nfwmv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-clone-response
           node-get-stream
           node-http-cache-semantics
           node-keyv
           node-lowercase-keys
           node-normalize-url
           node-responselike))
    (home-page "https://github.com/lukechilds/cacheable-request")
    (synopsis "Wrap native HTTP requests with RFC compliant cache support")
    (description
     "This package provides HTTP request caching with RFC 7234 compliance.")
    (license license:expat)))

(define-public node-got
  (package
    (name "node-got")
    (version "10.7.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/got/-/got-"
         version ".tgz"))
       (sha256
        (base32
         "04h2jfc5k1ivxfgcnjrm7qxa068ng4slqwm9zam93rwvkrpz0ihp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure)
          (add-after 'patch-dependencies 'remove-build-script
            (lambda _
              (substitute* "package.json"
                (("\"scripts\":[^}]*},") "")))))))
    (inputs
     (list node-cacheable-lookup
           node-cacheable-request
           node-decompress-response
           node-duplexer3
           node-get-stream
           node-lowercase-keys
           node-mimic-response
           node-p-cancelable
           node-p-event
           node-responselike
           node-sindresorhus-is
           node-szmarczak-http-timer
           node-to-readable-stream
           node-type-fest
           node-types-cacheable-request))
    (home-page "https://github.com/sindresorhus/got")
    (synopsis "Human-friendly and powerful HTTP request library for Node.js")
    (description
     "Got is a human-friendly and powerful HTTP request library for Node.js with a promise API, pagination, retries, and more.")
    (license license:expat)))

(define-public node-commander
  (package
    (name "node-commander")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/commander/-/commander-"
         version ".tgz"))
       (sha256
        (base32
         "1cxp6ibyy2y3756dl7bnab5wji64a73bg88v70n0w4gz1g54fh6x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/tj/commander.js")
    (synopsis "Complete solution for node.js command-line interfaces")
    (description
     "Commander is a light-weight, expressive, and powerful command-line framework for Node.js.")
    (license license:expat)))

(define-public node-color-name
  (package
    (name "node-color-name")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/color-name/-/color-name-"
         version ".tgz"))
       (sha256
        (base32
         "020p7x7k8rlph38lhsqpqvkx0b70lzlmk6mgal9r9sz8c527qysh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/colorjs/color-name")
    (synopsis "List of color names and values")
    (description
     "This package provides a list of color names mapped to their hex values.")
    (license license:expat)))

(define-public node-has-flag
  (package
    (name "node-has-flag")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/has-flag/-/has-flag-"
         version ".tgz"))
       (sha256
        (base32
         "1cdmvliwz8h02nwg0ipli0ydd1l82sz9s1m7bj5bn9yr24afp9vp"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/has-flag")
    (synopsis "Check if argv has a specific flag")
    (description
     "This package checks if argv has a specific flag.")
    (license license:expat)))

(define-public node-ansi-regex
  (package
    (name "node-ansi-regex")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/ansi-regex/-/ansi-regex-"
         version ".tgz"))
       (sha256
        (base32
         "1ng0r2k4mcz7b2bfr6g1dschnxm0vifaslsvv2smv06smb6ss3hf"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/chalk/ansi-regex")
    (synopsis "Regular expression for matching ANSI escape codes")
    (description
     "This package provides a regular expression for matching ANSI escape codes.")
    (license license:expat)))

(define-public node-emoji-regex
  (package
    (name "node-emoji-regex")
    (version "8.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/emoji-regex/-/emoji-regex-"
         version ".tgz"))
       (sha256
        (base32
         "01xi3ikahnlj77h2gqs3jb7kmnxn1nsb9dmnpvpqw288zgxxkk5m"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/mathiasbynens/emoji-regex")
    (synopsis "Regular expression for matching emoji")
    (description
     "This package provides a regular expression to match all Emoji-only symbols.")
    (license license:expat)))

(define-public node-is-fullwidth-code-point
  (package
    (name "node-is-fullwidth-code-point")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-"
         version ".tgz"))
       (sha256
        (base32
         "0jmw03rxmbwbrkx0a8wq05qsjhdrx9jn3vns88dhy1y6bnp5shbg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/is-fullwidth-code-point")
    (synopsis "Check if the character represented by a codepoint is fullwidth")
    (description
     "This package checks if the character represented by a given Unicode code point is fullwidth.")
    (license license:expat)))

(define-public node-camelcase
  (package
    (name "node-camelcase")
    (version "5.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/camelcase/-/camelcase-"
         version ".tgz"))
       (sha256
        (base32
         "15l68n2iq0ys0cf49h9adyvwk030kcqwrpalfcpmylc9p15342f6"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/camelcase")
    (synopsis "Convert string to camelCase")
    (description
     "This package converts dash/dot/underscore/space separated strings to camelCase.")
    (license license:expat)))

(define-public node-cli-boxes
  (package
    (name "node-cli-boxes")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/cli-boxes/-/cli-boxes-"
         version ".tgz"))
       (sha256
        (base32
         "0ym8jb2sjn2drwxjd8pvcxrs8xwx1n3wgwx77pdqg9b33lfshmq7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/cli-boxes")
    (synopsis "Boxes for use in the terminal")
    (description
     "This package provides boxes for use in the terminal.")
    (license license:expat)))

(define-public node-term-size
  (package
    (name "node-term-size")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/term-size/-/term-size-"
         version ".tgz"))
       (sha256
        (base32
         "0ikfkzjf58ccgnxhwjshr8lxr9na3a1ayqypphimgn7bzxg6dkky"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/term-size")
    (synopsis "Reliably get the terminal window size")
    (description
     "This package provides a reliable way to get the terminal window size.")
    (license license:expat)))

(define-public node-escape-goat
  (package
    (name "node-escape-goat")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/escape-goat/-/escape-goat-"
         version ".tgz"))
       (sha256
        (base32
         "0xjin2z7cnnhxg2rz5wdrd33b76q7ww8vyk6z5j6s89rc3jrl6rq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/escape-goat")
    (synopsis "Escape HTML entities")
    (description
     "This package provides utilities to escape and unescape HTML entities.")
    (license license:expat)))

(define-public node-is-npm
  (package
    (name "node-is-npm")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-npm/-/is-npm-"
         version ".tgz"))
       (sha256
        (base32
         "1sw76jcqc1m9ny5p6mk01sjilrn092parbsiymccnb6lmzrnap21"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/is-npm")
    (synopsis "Check if code is running as an npm script")
    (description
     "This package checks if your code is running as an npm script.")
    (license license:expat)))

(define-public node-has-yarn
  (package
    (name "node-has-yarn")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/has-yarn/-/has-yarn-"
         version ".tgz"))
       (sha256
        (base32
         "16l62xsvkb7nhvrljv61k8gyv0gqa1wjsma9ln77gf8p0sa2frfl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/has-yarn")
    (synopsis "Check if a project is using Yarn")
    (description
     "This package checks if a project is using Yarn.")
    (license license:expat)))

(define-public node-import-lazy
  (package
    (name "node-import-lazy")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/import-lazy/-/import-lazy-"
         version ".tgz"))
       (sha256
        (base32
         "1zb561xl5rw3a5gbvy0pm76kcrlxrrz63gcpgzq8kk068pi06p4j"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/import-lazy")
    (synopsis "Import modules lazily")
    (description
     "This package allows importing modules lazily.")
    (license license:expat)))

(define-public node-xdg-basedir
  (package
    (name "node-xdg-basedir")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/xdg-basedir/-/xdg-basedir-"
         version ".tgz"))
       (sha256
        (base32
         "1ya3ww7c00dcas4c6ybv8nl6ad0gl6qcsqwldpj6p4484s85yyzs"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/xdg-basedir")
    (synopsis "Get XDG Base Directory paths")
    (description
     "This package provides utilities to get XDG Base Directory paths.")
    (license license:expat)))

(define-public node-is-yarn-global
  (package
    (name "node-is-yarn-global")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-yarn-global/-/is-yarn-global-"
         version ".tgz"))
       (sha256
        (base32
         "1xa9p1h2id95vll9hr3jj816ll8grnpjmi9ml5hg1ffv8rxxral0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/LitoMore/is-yarn-global")
    (synopsis "Check if installed by Yarn globally")
    (description
     "This package checks if installed by Yarn globally.")
    (license license:expat)))

(define-public node-ci-info
  (package
    (name "node-ci-info")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/ci-info/-/ci-info-"
         version ".tgz"))
       (sha256
        (base32
         "1fan40rplhi85l0g37gvjxrhc8mj6yardf2d6rp1s85dal5z8n99"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/watson/ci-info")
    (synopsis "Get details about the current Continuous Integration environment")
    (description
     "This package provides information about the current CI environment.")
    (license license:expat)))


(define-public node-pupa
  (package
    (name "node-pupa")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/pupa/-/pupa-"
         version ".tgz"))
       (sha256
        (base32
         "0fsyb7h8f4ln2ba4x9386zafm08mx1lnxrkxj4qncn35c8msda1m"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-escape-goat))
    (home-page "https://github.com/sindresorhus/pupa")
    (synopsis "Simple micro templating")
    (description
     "This package provides simple micro templating.")
    (license license:expat)))

(define-public node-is-ci
  (package
    (name "node-is-ci")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-ci/-/is-ci-"
         version ".tgz"))
       (sha256
        (base32
         "17jb44xb61b331bh9nm5nv2jxayg74jjx048dcls03jrvff4i5hi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-ci-info))
    (home-page "https://github.com/watson/is-ci")
    (synopsis "Detect if code is running on a CI server")
    (description
     "This package detects whether the current environment is a CI server.")
    (license license:expat)))

(define-public node-color-convert
  (package
    (name "node-color-convert")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/color-convert/-/color-convert-"
         version ".tgz"))
       (sha256
        (base32
         "1qbw9rwfzcp7y0cpa8gmwlj7ccycf9pwn15zvf2s06f070ss83wj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-color-name))
    (home-page "https://github.com/Qix-/color-convert")
    (synopsis "Color conversion library")
    (description
     "This package provides color conversion utilities.")
    (license license:expat)))

(define-public node-ansi-styles
  (package
    (name "node-ansi-styles")
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/ansi-styles/-/ansi-styles-"
         version ".tgz"))
       (sha256
        (base32
         "0zwqsx67hr7m4a8dpd0jzkp2rjm5v7938x4rhcqh7djsv139llrc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-color-convert))
    (home-page "https://github.com/chalk/ansi-styles")
    (synopsis "ANSI escape codes for styling strings in the terminal")
    (description
     "This package provides ANSI escape codes for styling strings in the terminal.")
    (license license:expat)))

(define-public node-supports-color
  (package
    (name "node-supports-color")
    (version "7.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/supports-color/-/supports-color-"
         version ".tgz"))
       (sha256
        (base32
         "0jjyglzdzscmhgidn43zc218q5jf9h03hmaaq9h4wqil2vywlspi"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-has-flag))
    (home-page "https://github.com/chalk/supports-color")
    (synopsis "Detect whether a terminal supports color")
    (description
     "This package detects whether a terminal supports color.")
    (license license:expat)))

(define-public node-chalk
  (package
    (name "node-chalk")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/chalk/-/chalk-"
         version ".tgz"))
       (sha256
        (base32
         "0nvim1g2nqyjxmlqcb5ydjxf3lkvm9zcpg2nwbmylmla2a6xcx8x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-ansi-styles
           node-supports-color))
    (home-page "https://github.com/chalk/chalk")
    (synopsis "Terminal string styling done right")
    (description
     "This package provides terminal string styling.")
    (license license:expat)))

(define-public node-strip-ansi
  (package
    (name "node-strip-ansi")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/strip-ansi/-/strip-ansi-"
         version ".tgz"))
       (sha256
        (base32
         "1jh81jj6cn1lli1c7m6xi0ynra9zdghb1g63v1nib7zlpz87bnwv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-ansi-regex))
    (home-page "https://github.com/chalk/strip-ansi")
    (synopsis "Strip ANSI escape codes from a string")
    (description
     "This package strips ANSI escape codes from strings.")
    (license license:expat)))

(define-public node-string-width
  (package
    (name "node-string-width")
    (version "4.2.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/string-width/-/string-width-"
         version ".tgz"))
       (sha256
        (base32
         "0d19spdisrqxd6311fc7z1yg34ww6rwh1zxdk6pnk03fnaqlzfxd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-emoji-regex
           node-is-fullwidth-code-point
           node-strip-ansi))
    (home-page "https://github.com/sindresorhus/string-width")
    (synopsis "Get the visual width of a string")
    (description
     "This package gets the visual width of a string - the number of columns required to display it.")
    (license license:expat)))

(define-public node-ansi-align
  (package
    (name "node-ansi-align")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/ansi-align/-/ansi-align-"
         version ".tgz"))
       (sha256
        (base32
         "10phl2j4a0m5p03g0ph5bh6dhwbznc6sqw1i61bwxky1gf2cbbhw"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-string-width))
    (home-page "https://github.com/nexdrew/ansi-align")
    (synopsis "Align text with ANSI support")
    (description
     "This package provides text alignment with ANSI escape code support.")
    (license license:isc)))

(define-public node-widest-line
  (package
    (name "node-widest-line")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/widest-line/-/widest-line-"
         version ".tgz"))
       (sha256
        (base32
         "0ny97lblipnys9y955kdwai3xxfrmb7bzwiz4zka15rw497mc479"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-string-width))
    (home-page "https://github.com/sindresorhus/widest-line")
    (synopsis "Get the visual width of the widest line in a string")
    (description
     "This package gets the visual width of the widest line in a string.")
    (license license:expat)))

(define-public node-type-fest-0.8
  (package
    (name "node-type-fest-0.8")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/type-fest/-/type-fest-"
         version ".tgz"))
       (sha256
        (base32
         "1izmnb43sk4w2lq1849y0kr6qmz8hgjsyl6wi7y7amr80m0ny6cz"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/type-fest")
    (synopsis "Collection of essential TypeScript types")
    (description
     "This package provides a collection of essential TypeScript types.")
    (license license:expat)))

(define-public node-boxen
  (package
    (name "node-boxen")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/boxen/-/boxen-"
         version ".tgz"))
       (sha256
        (base32
         "1p79s29fc29mirhr2a69byji955v2m7ya0ida9zx7pkd0z0l51l0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-ansi-align
           node-camelcase
           node-chalk
           node-cli-boxes
           node-string-width
           node-term-size
           node-type-fest-0.8
           node-widest-line))
    (home-page "https://github.com/sindresorhus/boxen")
    (synopsis "Create boxes in the terminal")
    (description
     "This package creates boxes in the terminal.")
    (license license:expat)))

(define-public node-semver-diff
  (package
    (name "node-semver-diff")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/semver-diff/-/semver-diff-"
         version ".tgz"))
       (sha256
        (base32
         "0sqij9x6i1g2258rhndx9falid4c8n9ml7g278mr4xsdvvr8y2ci"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-semver))
    (home-page "https://github.com/sindresorhus/semver-diff")
    (synopsis "Get the diff type of two semver versions")
    (description
     "This package gets the diff type of two semver versions: major, minor, patch, etc.")
    (license license:expat)))

(define-public node-is-obj
  (package
    (name "node-is-obj")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-obj/-/is-obj-"
         version ".tgz"))
       (sha256
        (base32
         "06vnwn97ms8y6xjl9nck80whc1jqz2iby73n5z1qd41k8mv5m9xh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/is-obj")
    (synopsis "Check if a value is an object")
    (description
     "This package checks if a value is an object.")
    (license license:expat)))

(define-public node-crypto-random-string
  (package
    (name "node-crypto-random-string")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/crypto-random-string/-/crypto-random-string-"
         version ".tgz"))
       (sha256
        (base32
         "1angxzwxc9qiin2lbi5axp14aw2k3jdgbhc08fxp7h7hmnfk6s2m"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/crypto-random-string")
    (synopsis "Generate a cryptographically strong random string")
    (description
     "This package generates cryptographically strong random strings.")
    (license license:expat)))

(define-public node-imurmurhash
  (package
    (name "node-imurmurhash")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/imurmurhash/-/imurmurhash-"
         version ".tgz"))
       (sha256
        (base32
         "0q6bf91h2g5dhvcdss74sjvp5irimd97hp73jb8p2wvajqqs08xc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/jensyt/imurmurhash-js")
    (synopsis "Incremental MurmurHash3 hashing algorithm")
    (description
     "This package provides an incremental implementation of the MurmurHash3 algorithm.")
    (license license:expat)))

(define-public node-signal-exit
  (package
    (name "node-signal-exit")
    (version "3.0.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/signal-exit/-/signal-exit-"
         version ".tgz"))
       (sha256
        (base32
         "1a10ixkiak24yy6s7p9m7c6v9jkz2fm7wxgc2l3614dbdbx275j3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/tapjs/signal-exit")
    (synopsis "Fire an event on process exit")
    (description
     "This package fires an event when a process exits.")
    (license license:isc)))

(define-public node-is-typedarray
  (package
    (name "node-is-typedarray")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-typedarray/-/is-typedarray-"
         version ".tgz"))
       (sha256
        (base32
         "0i9qr2b79d0chhvpd1fc5pcp9bvirpg37f99d40alciqffmrfp0d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/inspect-js/is-typedarray")
    (synopsis "Detect if an object is a TypedArray")
    (description
     "This package detects whether an object is a TypedArray.")
    (license license:expat)))

(define-public node-typedarray-to-buffer
  (package
    (name "node-typedarray-to-buffer")
    (version "3.1.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/typedarray-to-buffer/-/typedarray-to-buffer-"
         version ".tgz"))
       (sha256
        (base32
         "0dpr3z61sijj5im122c581jr9iv5lgw0nvwyxswmgjw0ls24s9ib"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-is-typedarray))
    (home-page "https://github.com/feross/typedarray-to-buffer")
    (synopsis "Convert a typed array to a Buffer")
    (description
     "This package converts typed arrays to Buffers without a copy.")
    (license license:expat)))

(define-public node-dot-prop
  (package
    (name "node-dot-prop")
    (version "5.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/dot-prop/-/dot-prop-"
         version ".tgz"))
       (sha256
        (base32
         "0p76z3nwlibbcrl5md6ik6211nv0b6hf26phmqsdgv9lrvbj0gmg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-is-obj))
    (home-page "https://github.com/sindresorhus/dot-prop")
    (synopsis "Get, set, or delete a property from a nested object")
    (description
     "This package provides utilities to access nested object properties using dot notation.")
    (license license:expat)))

(define-public node-make-dir
  (package
    (name "node-make-dir")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/make-dir/-/make-dir-"
         version ".tgz"))
       (sha256
        (base32
         "1p3larbzfz9nny2m83x9isf5sng6gdc0x1vf6kyn24abln5anm7c"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-semver))
    (home-page "https://github.com/sindresorhus/make-dir")
    (synopsis "Make a directory and its parents if needed")
    (description
     "This package creates directories recursively like mkdir -p.")
    (license license:expat)))

(define-public node-unique-string
  (package
    (name "node-unique-string")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/unique-string/-/unique-string-"
         version ".tgz"))
       (sha256
        (base32
         "1dc7vv5pvkdn20jk4acimb432iias4jzy62acpk35nahxgksxglf"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-crypto-random-string))
    (home-page "https://github.com/sindresorhus/unique-string")
    (synopsis "Generate a unique random string")
    (description
     "This package generates a unique random string.")
    (license license:expat)))

(define-public node-write-file-atomic
  (package
    (name "node-write-file-atomic")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/write-file-atomic/-/write-file-atomic-"
         version ".tgz"))
       (sha256
        (base32
         "0rpsapa4i7gxhxaynnbf1xhjkz7n8y6xk9xn2q6nnfvidy2acq79"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-imurmurhash
           node-is-typedarray
           node-signal-exit
           node-typedarray-to-buffer))
    (home-page "https://github.com/npm/write-file-atomic")
    (synopsis "Atomically write files")
    (description
     "This package writes files atomically and handles errors.")
    (license license:isc)))

(define-public node-graceful-fs
  (package
    (name "node-graceful-fs")
    (version "4.2.11")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/graceful-fs/-/graceful-fs-"
         version ".tgz"))
       (sha256
        (base32
         "1709vla02prpbf34xqsvkqngvsmp5ypnljvg1pcgxrk1l553fq9r"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/isaacs/node-graceful-fs")
    (synopsis "Drop-in replacement for fs")
    (description
     "This package provides a drop-in replacement for the fs module with various improvements including better error handling.")
    (license license:isc)))

(define-public node-configstore
  (package
    (name "node-configstore")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/configstore/-/configstore-"
         version ".tgz"))
       (sha256
        (base32
         "1rgwjhvgylrf5lyfssjzybcqrj1xrss4wv6kymwnv70200297r85"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-dot-prop
           node-graceful-fs
           node-make-dir
           node-unique-string
           node-write-file-atomic
           node-xdg-basedir))
    (home-page "https://github.com/yeoman/configstore")
    (synopsis "Easily load and persist config")
    (description
     "This package easily loads and persists configuration without having to think about where and how.")
    (license license:bsd-2)))

(define-public node-ini
  (package
    (name "node-ini")
    (version "1.3.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/ini/-/ini-"
         version ".tgz"))
       (sha256
        (base32
         "0nk92bp5is23lsi1ip4qz5bjkzmjxkz9c1g79sx991ad212i37px"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/isaacs/ini")
    (synopsis "INI format parser and serializer")
    (description
     "This package parses and serializes INI format configuration files.")
    (license license:isc)))

(define-public node-minimist
  (package
    (name "node-minimist")
    (version "1.2.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/minimist/-/minimist-"
         version ".tgz"))
       (sha256
        (base32
         "10yfwkrl00d8gy9z622yrklg1jax3qk38j354jfw34xk2p0pc2im"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/minimistjs/minimist")
    (synopsis "Parse argument options")
    (description
     "This package parses command line argument options.")
    (license license:expat)))

(define-public node-deep-extend
  (package
    (name "node-deep-extend")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/deep-extend/-/deep-extend-"
         version ".tgz"))
       (sha256
        (base32
         "11hk1g7qjw9bj03c8y7v7n8p8mdfacpd9l8n57dga8qcj8s5zk0d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/unclechu/node-deep-extend")
    (synopsis "Recursive object extending")
    (description
     "This package provides recursive object extending.")
    (license license:expat)))

(define-public node-strip-json-comments
  (package
    (name "node-strip-json-comments")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/strip-json-comments/-/strip-json-comments-"
         version ".tgz"))
       (sha256
        (base32
         "16aq89q4gbs10fgy3a5n5miqphvs1sy44ckk4mf2dxqvmzmmzr6v"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/strip-json-comments")
    (synopsis "Strip comments from JSON")
    (description
     "This package strips comments from JSON files.")
    (license license:expat)))

(define-public node-rc
  (package
    (name "node-rc")
    (version "1.2.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/rc/-/rc-"
         version ".tgz"))
       (sha256
        (base32
         "0fz9r8aphj84cvxv8k0m7g008gffz561r6ryvljb8gi0hpjgm983"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-deep-extend
           node-ini
           node-minimist
           node-strip-json-comments))
    (home-page "https://github.com/dominictarr/rc")
    (synopsis "Configuration loader")
    (description
     "This package loads configuration from multiple sources.")
    (license license:expat)))

(define-public node-is-path-inside
  (package
    (name "node-is-path-inside")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-path-inside/-/is-path-inside-"
         version ".tgz"))
       (sha256
        (base32
         "0dipvy02ypbyz43gyvsp3hjgaqmxs4lpjzww2xlyj3x2wrgnb4gn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/sindresorhus/is-path-inside")
    (synopsis "Check if a path is inside another path")
    (description
     "This package checks if a path is inside another path.")
    (license license:expat)))

(define-public node-global-dirs
  (package
    (name "node-global-dirs")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/global-dirs/-/global-dirs-"
         version ".tgz"))
       (sha256
        (base32
         "0kz6aynsc7impkf0psfax09hkc41rmkzdxjxs18nzybnglvzf9qg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-ini))
    (home-page "https://github.com/sindresorhus/global-dirs")
    (synopsis "Get the directory of globally installed packages")
    (description
     "This package gets the directory of globally installed packages and binaries.")
    (license license:expat)))

(define-public node-registry-url
  (package
    (name "node-registry-url")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/registry-url/-/registry-url-"
         version ".tgz"))
       (sha256
        (base32
         "172s1ncsilaiswbmvdq0kkrnk88k7cgd54749w1yiyl008ih77rk"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-rc))
    (home-page "https://github.com/sindresorhus/registry-url")
    (synopsis "Get the set npm registry URL")
    (description
     "This package gets the configured npm registry URL.")
    (license license:expat)))

(define-public node-registry-auth-token
  (package
    (name "node-registry-auth-token")
    (version "4.2.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/registry-auth-token/-/registry-auth-token-"
         version ".tgz"))
       (sha256
        (base32
         "1j6kbx23gd2v3s8s7597zgx448fcc1l0q44ilb0z53si3yk4723i"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-rc))
    (home-page "https://github.com/rexxars/registry-auth-token")
    (synopsis "Get authentication token for npm registry")
    (description
     "This package gets the authentication token configured for an npm registry.")
    (license license:expat)))

(define-public node-package-json
  (package
    (name "node-package-json")
    (version "6.5.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/package-json/-/package-json-"
         version ".tgz"))
       (sha256
        (base32
         "013lg0fisjyv5wi8b5can7mby34wlkchi21i82slsdlig7v4w2nh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure)
          (add-after 'patch-dependencies 'remove-build-script
            (lambda _
              (substitute* "package.json"
                (("\"scripts\":[^}]*},") "")))))))
    (inputs
     (list node-got
           node-registry-auth-token
           node-registry-url
           node-semver))
    (home-page "https://github.com/sindresorhus/package-json")
    (synopsis "Get metadata of a package from npm registry")
    (description
     "This package gets the metadata of a package from the npm registry.")
    (license license:expat)))

(define-public node-latest-version
  (package
    (name "node-latest-version")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/latest-version/-/latest-version-"
         version ".tgz"))
       (sha256
        (base32
         "0lxhs8j64dmv38da0idzrysmr3c792g7ix7brls7x6kbxpcsjwv4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-package-json))
    (home-page "https://github.com/sindresorhus/latest-version")
    (synopsis "Get the latest version of an npm package")
    (description
     "This package gets the latest version of an npm package.")
    (license license:expat)))

(define-public node-is-installed-globally
  (package
    (name "node-is-installed-globally")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/is-installed-globally/-/is-installed-globally-"
         version ".tgz"))
       (sha256
        (base32
         "0s94csvq7x5ycxsmm3irr2js0iw89c9841790xrcx3v1lnjqragy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-global-dirs
           node-is-path-inside))
    (home-page "https://github.com/sindresorhus/is-installed-globally")
    (synopsis "Check if package is installed globally")
    (description
     "This package checks if your package is installed globally.")
    (license license:expat)))

(define-public node-update-notifier
  (package
    (name "node-update-notifier")
    (version "4.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/update-notifier/-/update-notifier-"
         version ".tgz"))
       (sha256
        (base32
         "0ynnzcv5sjbin8l05l827fsd0i0pzcjzhlkdd0axj718jz3m2kdz"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-boxen
           node-chalk
           node-configstore
           node-has-yarn
           node-import-lazy
           node-is-ci
           node-is-installed-globally
           node-is-npm
           node-is-yarn-global
           node-latest-version
           node-pupa
           node-semver-diff
           node-xdg-basedir))
    (home-page "https://github.com/yeoman/update-notifier")
    (synopsis "Update notifications for CLI apps")
    (description
     "This package provides update notifications for your CLI app.")
    (license license:bsd-2)))

(define-public node-open-swagger-ui
  (package
    (name "node-open-swagger-ui")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/open-swagger-ui/-/open-swagger-ui-"
         version ".tgz"))
       (sha256
        (base32
         "1pb12b712dnbzxwj3wbj9nwkkdsjqjmsm80n4vxhfcwz5n55hzqd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-commander
           node-express
           node-get-port
           node-got
           node-is-url
           node-js-yaml
           node-lodash-isplainobject
           node-open
           node-ora
           node-swagger-ui-express
           node-update-notifier
           node-upath))
    (home-page "https://github.com/kimamula/open-swagger-ui")
    (synopsis "Open Swagger UI from CLI")
    (description
     "This package opens Swagger UI in a browser from the command line.")
    (license license:expat)))

(define-public node-swagger-ui-dist
  (package
    (name "node-swagger-ui-dist")
    (version "3.52.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/swagger-ui-dist/-/swagger-ui-dist-"
         version ".tgz"))
       (sha256
        (base32
         "1wi49syngr6cbi7b5ysy53ryck1j4lagl3s1k8z4aa2hs922q4zg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (home-page "https://github.com/swagger-api/swagger-ui")
    (synopsis "Swagger UI distribution files")
    (description
     "This package contains the distribution files for Swagger UI, a tool for
visualizing and interacting with API resources without having any of the
implementation logic in place.")
    (license license:asl2.0)))

(define-public node-swagger-ui-express
  (package
    (name "node-swagger-ui-express")
    (version "4.1.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/swagger-ui-express/-/swagger-ui-express-"
         version ".tgz"))
       (sha256
        (base32
         "0grgbaqbzbnwd747nsskpqpxb2blxpmhmybv680w3cwiwhg1alm2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'configure))))
    (inputs
     (list node-swagger-ui-dist))
    (propagated-inputs
     (list node-express))
    (home-page "https://github.com/scottie1984/swagger-ui-express")
    (synopsis "Swagger UI Express middleware")
    (description
     "This package provides middleware for Express to serve auto-generated
swagger-ui API documentation based on a swagger.json file.")
    (license license:expat)))

(define-public node-anthropic-ai-claude-code
  (package
    (name "node-anthropic-ai-claude-code")
    (version "2.0.13")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://registry.npmjs.org/@anthropic-ai/claude-code/"
         "-/claude-code-" version ".tgz"))
       (sha256
        (base32
         "08cdcgx82h5gv2adzzdgggiav3mkzvd058gycxxrbll4hn4av5br"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'validate-runpath))))
    (home-page "https://github.com/anthropics/claude-code")
    (synopsis
     "AI coding assistant that lives in your terminal")
    (description
     "Claude Code is an agentic coding tool that lives in your terminal.
It understands your codebase, edits files, runs terminal commands, and
handles entire workflows through natural language commands.  Powered by
Anthropic's Claude AI assistant.")
    (license
     (license:non-copyleft
      "https://github.com/anthropics/claude-code/blob/main/LICENSE.md"
      "See LICENSE.md in the repository."))))
