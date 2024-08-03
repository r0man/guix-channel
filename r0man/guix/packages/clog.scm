(define-module (r0man guix packages clog)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages wm)
  #:use-module (guix build-system)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public sbcl-lack
  (let ((commit "35d3a8e03cab9204eec88c7dfe4d5366fc2ea922")
        (revision "2"))
    (package
      (name "sbcl-lack")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/lack")
               (commit commit)))
         (file-name (git-file-name "lack" version))
         (sha256
          (base32 "1yrhhzn8ywdjxwpaxzlnsm2lslhy45r89brn8gh5n08mdyjlp4l2"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-prove))
      (inputs
       `(("circular-streams" ,sbcl-circular-streams)
         ("cl-isaac" ,sbcl-cl-isaac)
         ("http-body" ,sbcl-http-body)
         ("ironclad" ,sbcl-ironclad)
         ("local-time" ,sbcl-local-time)
         ("quri" ,sbcl-quri)
         ("trivial-mimes" ,sbcl-trivial-mimes)
         ("trivial-rfc-1123" ,sbcl-trivial-rfc-1123)))
      (arguments
       '(#:asd-systems '("lack"
                         "lack-request"
                         "lack-response"
                         "lack-component"
                         "lack-util"
                         "lack-util-writer-stream"
                         "lack-middleware-backtrace"
                         "lack-middleware-static")
         ;; XXX: Component :CLACK not found
         #:tests? #f))
      (home-page "https://github.com/fukamachi/lack")
      (synopsis "Lack, the core of Clack")
      (description
       "Lack is a Common Lisp library which allows web applications to be
constructed of modular components.  It was originally a part of Clack, however
it's going to be rewritten as an individual project since Clack v2 with
performance and simplicity in mind.")
      (license license:llgpl))))

(define-public sbcl-clog
  (package
    (name "sbcl-clog")
    (version "2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rabbibotton/clog")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cl-clog" version))
       (sha256
        (base32 "175zb93kxnxv0hr8435mkm94fqhjq51wq55ybd55kcyk5y5h2xaf"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-3bmd
           sbcl-atomics
           sbcl-alexandria
           sbcl-bordeaux-threads
           sbcl-cl-isaac
           sbcl-cl-ppcre
           sbcl-cl-sqlite
           sbcl-cl-template
           sbcl-clack
           sbcl-closer-mop
           sbcl-colorize
           sbcl-dbi
           sbcl-hunchentoot
           sbcl-lack
           sbcl-mgl-pax
           sbcl-parse-float
           sbcl-quri
           sbcl-trivial-open-browser
           sbcl-websocket-driver
           sbcl-cl-pass
           sbcl-lack
           ;; #:lack-middleware-static
           ;; #:lack-request
           ;; #:lack-util-writer-stream
           sbcl-cl-sqlite
           sbcl-trivial-gray-streams

           ))
    (arguments
     '(#:asd-systems '("clog" "clog/docs" "clog/tools")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-symbol-name
                    (lambda _
                      (substitute* "source/clog-docs.lisp"
                        (("clog:@CLOG-MANUAL")
                         "clog::@CLOG_MANUAL")))))))
    (home-page "https://github.com/rabbibotton/clog")
    (synopsis "Common Lisp Omnificent GUI")
    (description
     "This package provides a Common Lisp web framework for building GUI
applications.  CLOG can take the place, or work along side, most cross platform
GUI frameworks and website frameworks.  The CLOG package starts up the
connectivity to the browser or other websocket client (often a browser embedded
in a native template application).")
    (license license:bsd-3)))

(define-public cl-clog
  (sbcl-package->cl-source-package sbcl-clog))
