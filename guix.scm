;;; guix.scm --- the development environment of this channel
;;;
;;; Run the test suite from a self-contained (pure) environment built from
;;; this file:
;;;
;;;   guix shell -D -f ./guix.scm -- make check
;;;
;;; Note the leading `./': with a bare `guix.scm' argument, `guix shell'
;;; treats the file as a manifest of packages to install instead of as the
;;; package whose development inputs to assemble, and the environment then
;;; lacks guile and GUILE_LOAD_PATH.
;;;
;;; The file returns one package, `guile-r0man-channel', whose development
;;; inputs are the whole environment: guile-next, guix and the nonguix
;;; modules (so the modules of this channel and of the modules it imports
;;; resolve), the autotools and texinfo, and the tools the tests drive
;;; (coreutils, git, util-linux).

(use-modules
 ((guix licenses) #:prefix license:)
 (gnu packages autotools)
 (gnu packages base)
 (gnu packages gettext)
 (gnu packages guile)
 (gnu packages linux)
 (gnu packages package-management)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages version-control)
 (guix build-system copy)
 (guix build-system gnu)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (guix utils)
 (srfi srfi-1))

;; Package nonguix's channel modules as a regular Guix package so they
;; are available inside `guix shell --container --pure'.  Without this,
;; `(use-modules (nonguix build-system binary))' fails to resolve in
;; the dev shell — the nonguix channel is only on the load path when
;; consumed via `guix pull'.
(define-public guile-nonguix
  (let ((commit "c4541fdb0b472664dafe5d7b1ec2e51e4ef7b772")
        (revision "0"))
    (package
      (name "guile-nonguix")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/nonguix/nonguix")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1v50pvwr65hn8h09sc0hg9lz2vwzywagmyvkjbxs6v68gwy1gi3f"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("nonguix"     "share/guile/site/3.0/nonguix")
           ("nongnu"      "share/guile/site/3.0/nongnu")
           ("nonguix.scm" "share/guile/site/3.0/nonguix.scm"))))
      ;; Declare the Guile search path this package installs into, so a
      ;; profile that contains it (a `guix shell -D' development profile,
      ;; for example) puts the modules on GUILE_LOAD_PATH instead of
      ;; requiring the host's channel installation.
      (native-search-paths (package-native-search-paths guile-3.0))
      (synopsis "nonguix channel modules as a Guix package")
      (description "Provides the @code{(nonguix …)} and @code{(nongnu …)}
Guile modules from the nonguix channel, packaged for use as a dev-shell
input where the channel mechanism isn't available — for example inside
@code{guix shell --container --pure}.")
      (home-page "https://gitlab.com/nonguix/nonguix")
      (license license:gpl3+))))

(define-public guile-r0man-channel
  (package
    (name "guile-r0man-channel")
    (version "0.1")
    (source
     (local-file
      (dirname (current-filename))
      #:recursive? #t
      #:select?
      (λ (file stat)
        (not (any (lambda (s) (string-contains file s))
                  (list ".git" ".dir-locals.el" "guix.scm"))))))
    (build-system gnu-build-system)
    ;; Installing the package (rather than only its development inputs) puts
    ;; the channel's own modules on GUILE_LOAD_PATH.
    (native-search-paths (package-native-search-paths guile-next))
    (native-inputs
     (list autoconf
           automake
           ;; The test suite shells out to coreutils, and drives git and
           ;; (for the root activation scenarios, inside an unprivileged
           ;; user namespace) unshare/mount, none of which the development
           ;; inputs otherwise provide under `guix shell --pure'.
           coreutils
           git
           gnu-gettext
           guile-next
           guile-nonguix
           guix
           pkg-config
           texinfo
           util-linux))
    (inputs (list guile-next))
    (synopsis "r0man Guix channel")
    (description "Personal Guix channel by r0man.")
    (home-page "https://github.com/r0man/guix-channel")
    (license license:gpl3+)))

guile-r0man-channel
