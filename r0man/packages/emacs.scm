(define-module (r0man packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-aio
  (package
    (name "emacs-aio")
    (version "20200610.1904")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skeeto/emacs-aio.git")
             (commit "da93523e235529fa97d6f251319d9e1d6fc24a41")))
       (sha256
        (base32 "0hnxbz5pxlrgxhjr5gnhf06qwg67g5pd87xkp0smmagsh18pnf76"))))
    (build-system emacs-build-system)
    (arguments
     '(#:include '("^aio.el$" "^README.md$" "^UNLICENSE$") #:exclude '()))
    (home-page "https://github.com/skeeto/emacs-aio")
    (synopsis "async/await for Emacs Lisp")
    (description
     "`aio` is to Emacs Lisp as [`asyncio`][asyncio] is to Python.  This package
builds upon Emacs 25 generators to provide functions that pause while they wait
on asynchronous events.  They do not block any thread while paused.

The main components of this package are `aio-defun' / `aio-lambda' to define
async function, and `aio-await' to pause these functions while they wait on
asynchronous events.  When an asynchronous function is paused, the main thread
is not blocked.  It is no more or less powerful than callbacks, but is nicer to
use.

This is implementation is based on Emacs 25 generators, and asynchronous
functions are actually iterators in disguise, operated as stackless, asymmetric
coroutines.")
    (license #f)))

(define-public emacs-scss-mode
  (package
    (name "emacs-scss-mode")
    (version "20180123.1708")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/antonj/scss-mode.git")
             (commit "cf58dbec5394280503eb5502938f3b5445d1b53d")))
       (sha256
        (base32 "0raja19l0igwr0pn0ghr1pj1d8i9k3m3764ma4r8nwzxcj9qw4ja"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/antonj/scss-mode")
    (synopsis "Major mode for editing SCSS files")
    (description
     "Command line utility sass is required, see http://sass-lang.com/ To install
sass: gem install sass

Also make sure sass location is in emacs PATH, example: (setq exec-path (cons
(expand-file-name \"~/.gem/ruby/1.8/bin\") exec-path)) or customize\n`scss-sass-command' to point to your sass executable.")
    (license #f)))
