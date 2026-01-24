(define-module (r0man guix packages java)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define* (make-graalvm-ce version hash)
  "Create a GraalVM Community Edition package for the given VERSION and HASH."
  (package
    (name "graalvm-ce")
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/graalvm/graalvm-ce-builds/releases/download/"
             "jdk-" version "/graalvm-community-jdk-" version
             "_linux-x64_bin.tar.gz"))
       (sha256 (base32 hash))))
    (build-system copy-build-system)
    (arguments
     (list
      #:validate-runpath? #f  ; Optional X11/audio libs not always available
      #:install-plan
      #~`(("." "./" #:exclude ("GRAALVM-README.md"
                               "LICENSE.txt"
                               "NOTICE.txt"
                               "license-information-user-manual.zip")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'unpack-tarball
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "xzf" source "--strip-components=1")))
          (add-after 'install 'patch-elf-files
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (ice-9 ftw)
                           (ice-9 popen)
                           (ice-9 rdelim))
              (let* ((out (assoc-ref outputs "out"))
                     (glibc (assoc-ref inputs "glibc"))
                     (gcc-lib (assoc-ref inputs "gcc"))
                     (zlib (assoc-ref inputs "zlib"))
                     (ld-linux (string-append glibc
                                              "/lib/ld-linux-x86-64.so.2"))
                     (rpath (string-join
                             (list (string-append out "/lib")
                                   (string-append out "/lib/server")
                                   (string-append gcc-lib "/lib")
                                   (string-append glibc "/lib")
                                   (string-append zlib "/lib"))
                             ":")))
                (define (elf-file? file)
                  "Check if FILE is an ELF binary using file magic bytes."
                  (and (file-exists? file)
                       (not (symbolic-link? file))
                       (let ((port (open-file file "rb")))
                         (dynamic-wind
                           (lambda () #t)
                           (lambda ()
                             (let ((b0 (read-char port))
                                   (b1 (read-char port))
                                   (b2 (read-char port))
                                   (b3 (read-char port)))
                               (and (not (eof-object? b0))
                                    (char=? b0 (integer->char #x7f))
                                    (char=? b1 #\E)
                                    (char=? b2 #\L)
                                    (char=? b3 #\F))))
                           (lambda () (close-port port))))))
                (define (patch-elf file)
                  "Patch ELF file with interpreter and rpath."
                  (let* ((port (open-pipe* OPEN_READ "patchelf"
                                           "--print-interpreter" file))
                         (has-interp (not (eof-object? (read-line port)))))
                    (close-pipe port)
                    ;; Set interpreter if file has one (executables)
                    (when has-interp
                      (invoke "patchelf" "--set-interpreter"
                              ld-linux file))
                    ;; Always set rpath
                    (invoke "patchelf" "--set-rpath" rpath file)))
                (define (process-directory dir)
                  "Process all ELF files in DIR recursively."
                  (define native-image-bin
                    (string-append out "/lib/svm/bin/native-image"))
                  (ftw dir
                       (lambda (path statinfo flag)
                         (when (and (eq? flag 'regular)
                                    (elf-file? path)
                                    ;; Skip native-image, handled separately
                                    (not (string=? path native-image-bin)))
                           (format #t "Patching: ~a~%" path)
                           (catch #t
                             (lambda () (patch-elf path))
                             (lambda (key . args)
                               (format #t "Warning: failed to patch ~a~%"
                                       path))))
                         #t)))
                ;; Patch all ELF files
                (process-directory out))))
          (add-after 'patch-elf-files 'wrap-native-image
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (ice-9 ftw)
                           (ice-9 match))
              (let* ((out (assoc-ref outputs "out"))
                     (glibc (assoc-ref inputs "glibc"))
                     (gcc-lib (assoc-ref inputs "gcc"))
                     (gcc-toolchain (assoc-ref inputs "gcc-toolchain"))
                     (binutils (assoc-ref inputs "binutils"))
                     (linux-headers (assoc-ref inputs "linux-libre-headers"))
                     (zlib (assoc-ref inputs "zlib"))
                     (bash (search-input-file inputs "/bin/bash"))
                     (ld-linux (string-append glibc
                                              "/lib/ld-linux-x86-64.so.2"))
                     (native-image-bin
                      (string-append out "/lib/svm/bin/native-image"))
                     (native-image-wrapper
                      (string-append out "/bin/native-image"))
                     ;; Find GCC include paths dynamically
                     (gcc-lib-dir (string-append gcc-lib "/lib/gcc"))
                     (gcc-include-dir
                      (let ((dirs '()))
                        (ftw gcc-lib-dir
                             (lambda (path statinfo flag)
                               (when (and (eq? flag 'directory)
                                          (string-suffix? "/include" path))
                                 (set! dirs (cons path dirs)))
                               #t))
                        (car (filter (lambda (d)
                                       (not (string-contains d "include-fixed")))
                                     dirs))))
                     (gcc-fixed-include-dir
                      (string-append (dirname gcc-include-dir)
                                     "/include-fixed")))
                ;; Patch native-image interpreter only (no rpath due to
                ;; glibc symbol issues - uses LD_LIBRARY_PATH instead)
                (invoke "patchelf" "--set-interpreter" ld-linux
                        native-image-bin)
                ;; Remove symlink and create wrapper
                (when (file-exists? native-image-wrapper)
                  (delete-file native-image-wrapper))
                (call-with-output-file native-image-wrapper
                  (lambda (port)
                    (format port "#!~a~%" bash)
                    (format port "export PATH=\"~a/bin:~a/bin${PATH:+:}$PATH\"~%"
                            gcc-toolchain binutils)
                    (format port "~a~%"
                            (string-append
                             "export LIBRARY_PATH=\""
                             glibc "/lib:" zlib "/lib:" gcc-toolchain "/lib"
                             "${LIBRARY_PATH:+:}$LIBRARY_PATH\""))
                    (format port "~a~%"
                            (string-append
                             "export C_INCLUDE_PATH=\""
                             glibc "/include:" linux-headers "/include:"
                             zlib "/include:" gcc-toolchain "/include"
                             "${C_INCLUDE_PATH:+:}$C_INCLUDE_PATH\""))
                    (format port "~a~%"
                            (string-append
                             "export LD_LIBRARY_PATH=\""
                             out "/lib:" out "/lib/server:"
                             glibc "/lib:" gcc-lib "/lib:" zlib "/lib"
                             "${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH\""))
                    (format port "exec \"~a\" \\~%" native-image-bin)
                    (format port "  -H:+UnlockExperimentalVMOptions \\~%")
                    (format port "  \"-H:CCompilerOption=-I~a\" \\~%"
                            gcc-include-dir)
                    (format port "  \"-H:CCompilerOption=-I~a\" \\~%"
                            gcc-fixed-include-dir)
                    (format port "  \"-H:CCompilerOption=-I~a/include\" \\~%"
                            glibc)
                    (format port "  \"-H:CCompilerOption=-I~a/include\" \\~%"
                            linux-headers)
                    (format port "  \"-H:CLibraryPath=~a/lib\" \\~%" glibc)
                    (format port "  \"-H:CLibraryPath=~a/lib\" \\~%" zlib)
                    (format port "  \"$@\"~%")))
                (chmod native-image-wrapper #o755)))))))
    (native-inputs (list patchelf tar))
    (inputs
     (list `(,gcc "lib")
           bash-minimal
           binutils
           gcc-toolchain
           glibc
           linux-libre-headers
           zlib))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.graalvm.org/")
    (synopsis "High-performance JDK with native-image ahead-of-time compiler")
    (description
     "GraalVM is a high-performance JDK distribution that includes the
GraalVM compiler, which can be used as a just-in-time (JIT) compiler for
the JVM, and the native-image tool for ahead-of-time compilation of Java
applications to native executables.  This package provides the Community
Edition of GraalVM.")
    (license license:gpl2+)))

(define-public graalvm-ce-21
  (make-graalvm-ce "21.0.2" "0j5ffszcaqv3fq159hyb611jm8w1q4n1cywmbd7vi69smad0cj5h"))

(define-public graalvm-ce-24
  (make-graalvm-ce "24.0.2" "080774x1chpa0n8bpmw575g5myi63ikffwgcvq3r7nvdh9n88qkd"))

(define-public graalvm-ce-25
  (make-graalvm-ce "25.0.2" "1w3ac0cl9d2ja98klyq5f1hp6j9ixx7q5jx0n2v06kfsiwf7kgp0"))

;; Default to the latest stable release (JDK 24)
(define-public graalvm-ce graalvm-ce-25)
