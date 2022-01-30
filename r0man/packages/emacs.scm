(define-module (r0man packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz))

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
