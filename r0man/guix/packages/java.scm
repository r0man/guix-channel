(define-module (r0man guix packages java)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nonguix build-system binary))

(define-public graalvm-ce
  (package
    (name "graalvm-ce")
    (version "25.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://download.oracle.com/graalvm/25/latest/graalvm-jdk-25_linux-x64_bin.tar.gz")
       (sha256
        (base32 "0dqq8dy7psij36py304q0x4hs79f4hfgkzbl6gq3kri922x05ayl"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f  ;; GraalVM libs have internal dependencies
      #:patchelf-plan
      #~`(;; Main executables (excluding symlinks)
          ("bin/java" ("gcc" "glibc" "zlib"))
          ("bin/javac" ("gcc" "glibc" "zlib"))
          ("bin/javap" ("gcc" "glibc" "zlib"))
          ("bin/javadoc" ("gcc" "glibc" "zlib"))
          ("bin/jar" ("gcc" "glibc" "zlib"))
          ("bin/jarsigner" ("gcc" "glibc" "zlib"))
          ("bin/jcmd" ("gcc" "glibc" "zlib"))
          ("bin/jconsole" ("gcc" "glibc" "zlib"))
          ("bin/jdb" ("gcc" "glibc" "zlib"))
          ("bin/jdeprscan" ("gcc" "glibc" "zlib"))
          ("bin/jdeps" ("gcc" "glibc" "zlib"))
          ("bin/jfr" ("gcc" "glibc" "zlib"))
          ("bin/jhsdb" ("gcc" "glibc" "zlib"))
          ("bin/jimage" ("gcc" "glibc" "zlib"))
          ("bin/jinfo" ("gcc" "glibc" "zlib"))
          ("bin/jlink" ("gcc" "glibc" "zlib"))
          ("bin/jmap" ("gcc" "glibc" "zlib"))
          ("bin/jmod" ("gcc" "glibc" "zlib"))
          ("bin/jnativescan" ("gcc" "glibc" "zlib"))
          ("bin/jpackage" ("gcc" "glibc" "zlib"))
          ("bin/jps" ("gcc" "glibc" "zlib"))
          ("bin/jrunscript" ("gcc" "glibc" "zlib"))
          ("bin/jshell" ("gcc" "glibc" "zlib"))
          ("bin/jstack" ("gcc" "glibc" "zlib"))
          ("bin/jstat" ("gcc" "glibc" "zlib"))
          ("bin/jstatd" ("gcc" "glibc" "zlib"))
          ("bin/jwebserver" ("gcc" "glibc" "zlib"))
          ("bin/keytool" ("gcc" "glibc" "zlib"))
          ("bin/native-image-inspect" ("gcc" "glibc" "zlib"))
          ("bin/rmiregistry" ("gcc" "glibc" "zlib"))
          ("bin/serialver" ("gcc" "glibc" "zlib"))
          ;; Shared libraries
          ("lib/jspawnhelper" ("gcc" "glibc" "zlib"))
          ("lib/libjli.so" ("gcc" "glibc" "zlib"))
          ("lib/libjava.so" ("gcc" "glibc" "zlib"))
          ("lib/libjimage.so" ("gcc" "glibc" "zlib"))
          ("lib/libjsig.so" ("gcc" "glibc" "zlib"))
          ("lib/libnet.so" ("gcc" "glibc" "zlib"))
          ("lib/libnio.so" ("gcc" "glibc" "zlib"))
          ("lib/libverify.so" ("gcc" "glibc" "zlib"))
          ("lib/libzip.so" ("gcc" "glibc" "zlib"))
          ("lib/libawt.so" ("gcc" "glibc" "zlib"))
          ("lib/libawt_headless.so" ("gcc" "glibc" "zlib"))
          ("lib/libawt_xawt.so" ("gcc" "glibc" "zlib"))
          ("lib/libfontmanager.so" ("gcc" "glibc" "zlib"))
          ("lib/libjavajpeg.so" ("gcc" "glibc" "zlib"))
          ("lib/libjawt.so" ("gcc" "glibc" "zlib"))
          ("lib/libjsound.so" ("gcc" "glibc" "zlib"))
          ("lib/liblcms.so" ("gcc" "glibc" "zlib"))
          ("lib/libmlib_image.so" ("gcc" "glibc" "zlib"))
          ("lib/libsplashscreen.so" ("gcc" "glibc" "zlib"))
          ("lib/libinstrument.so" ("gcc" "glibc" "zlib"))
          ("lib/libmanagement.so" ("gcc" "glibc" "zlib"))
          ("lib/libprefs.so" ("gcc" "glibc" "zlib"))
          ("lib/librmi.so" ("gcc" "glibc" "zlib"))
          ("lib/libj2gss.so" ("gcc" "glibc" "zlib"))
          ("lib/libj2pcsc.so" ("gcc" "glibc" "zlib"))
          ("lib/libattach.so" ("gcc" "glibc" "zlib"))
          ("lib/libj2pkcs11.so" ("gcc" "glibc" "zlib"))
          ("lib/libsaproc.so" ("gcc" "glibc" "zlib"))
          ("lib/libjsvml.so" ("gcc" "glibc" "zlib"))
          ("lib/libdt_socket.so" ("gcc" "glibc" "zlib"))
          ("lib/libjdwp.so" ("gcc" "glibc" "zlib"))
          ("lib/libmanagement_ext.so" ("gcc" "glibc" "zlib"))
          ("lib/libmanagement_agent.so" ("gcc" "glibc" "zlib"))
          ("lib/libextnet.so" ("gcc" "glibc" "zlib"))
          ("lib/libsctp.so" ("gcc" "glibc" "zlib"))
          ("lib/libjaas.so" ("gcc" "glibc" "zlib"))
          ("lib/libsimdsort.so" ("gcc" "glibc" "zlib"))
          ("lib/libsyslookup.so" ("gcc" "glibc" "zlib"))
          ("lib/libjvmcicompiler.so" ("gcc" "glibc" "zlib"))
          ("lib/libnative-image-agent.so" ("gcc" "glibc" "zlib"))
          ("lib/libnative-image-diagnostics-agent.so" ("gcc" "glibc" "zlib"))
          ("lib/server/libjsig.so" ("gcc" "glibc" "zlib"))
          ("lib/server/libjvm.so" ("gcc" "glibc" "zlib"))
          ;; SVM components - native-image is NOT patched (glibc symbol issues)
          ;; It's invoked via ld-linux wrapper instead
          ("lib/svm/builder/lib/libreporterchelper.so" ("gcc" "glibc" "zlib")))
      #:install-plan
      #~`(("." "./" #:exclude ("GRAALVM-README.md"
                               "LICENSE.txt"
                               "NOTICE.txt"
                               "license-information-user-manual.zip")))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              ;; Extract and strip the top-level directory
              (invoke "tar" "xzf" source "--strip-components=1")))
          (add-after 'install 'fix-internal-rpaths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (ice-9 popen)
                           (ice-9 rdelim)
                           (ice-9 regex))
              (define (get-interpreter file)
                (let* ((port (open-pipe* OPEN_READ "patchelf" "--print-interpreter" file))
                       (interp (read-line port)))
                  (close-pipe port)
                  (if (eof-object? interp) #f interp)))
              ;; Get glibc path from the interpreter set by patchelf phase
              ;; to ensure consistency between interpreter and RPATH
              (define (glibc-from-interpreter interp)
                (and interp
                     (let ((m (string-match "/gnu/store/[^/]+-glibc-[^/]+" interp)))
                       (and m (match:substring m)))))
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib"))
                     (lib-server (string-append out "/lib/server"))
                     (gcc-lib (assoc-ref inputs "gcc"))
                     ;; Get glibc from already-patched java binary for consistency
                     (java-interp (get-interpreter (string-append out "/bin/java")))
                     (glibc (or (glibc-from-interpreter java-interp)
                                (assoc-ref inputs "glibc")))
                     (zlib (assoc-ref inputs "zlib")))
                ;; Add internal lib paths to executables
                (for-each
                 (lambda (file)
                   (let ((path (string-append out "/bin/" file)))
                     (when (and (file-exists? path)
                                (not (symbolic-link? path)))
                       (invoke "patchelf" "--set-rpath"
                               (string-append lib ":" lib-server ":"
                                             gcc-lib "/lib:"
                                             glibc "/lib:"
                                             zlib "/lib")
                               path))))
                 '("java" "javac" "javap" "javadoc" "jar" "jarsigner"
                   "jcmd" "jconsole" "jdb" "jdeprscan" "jdeps" "jfr"
                   "jhsdb" "jimage" "jinfo" "jlink" "jmap" "jmod"
                   "jnativescan" "jpackage" "jps" "jrunscript" "jshell"
                   "jstack" "jstat" "jstatd" "jwebserver" "keytool"
                   "native-image-inspect" "rmiregistry" "serialver"))
                ;; Add internal lib paths to shared libraries
                (for-each
                 (lambda (file)
                   (let ((path (string-append lib "/" file)))
                     (when (file-exists? path)
                       (invoke "patchelf" "--set-rpath"
                               (string-append lib ":" lib-server ":"
                                             gcc-lib "/lib:"
                                             glibc "/lib:"
                                             zlib "/lib")
                               path))))
                 '("jspawnhelper" "libjli.so" "libjava.so" "libjimage.so"
                   "libjsig.so" "libnet.so" "libnio.so" "libverify.so"
                   "libzip.so"))
                ;; Note: native-image is NOT patched due to glibc symbol issues
                ;; It's wrapped to invoke via ld-linux instead
                )))
          ;; Patch native-image interpreter only (no RPATH) and create wrapper
          ;; with LD_LIBRARY_PATH. Direct execution allows /proc/self/exe
          ;; to work correctly for home directory detection.
          (add-after 'fix-internal-rpaths 'patch-native-image
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (ice-9 popen)
                           (ice-9 rdelim)
                           (ice-9 regex))
              (let* ((out (assoc-ref outputs "out"))
                     (glibc (assoc-ref inputs "glibc"))
                     (ld-linux (string-append glibc "/lib/ld-linux-x86-64.so.2"))
                     (native-image-bin
                      (string-append out "/lib/svm/bin/native-image")))
                ;; Patch only the interpreter, not RPATH
                ;; This allows native-image to run directly so /proc/self/exe
                ;; returns the correct path for home directory detection
                (invoke "patchelf" "--set-interpreter" ld-linux
                        native-image-bin))))
          (add-after 'patch-native-image 'wrap-native-image
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (ice-9 popen)
                           (ice-9 rdelim))
              (let* ((out (assoc-ref outputs "out"))
                     (glibc (assoc-ref inputs "glibc"))
                     (gcc-lib (assoc-ref inputs "gcc"))
                     (gcc-toolchain (assoc-ref inputs "gcc-toolchain"))
                     (binutils (assoc-ref inputs "binutils"))
                     (linux-headers (assoc-ref inputs "linux-libre-headers"))
                     (zlib (assoc-ref inputs "zlib"))
                     (bash (search-input-file inputs "/bin/bash"))
                     (native-image-bin
                      (string-append out "/lib/svm/bin/native-image"))
                     (native-image-wrapper
                      (string-append out "/bin/native-image"))
                     ;; Get gcc's internal include paths
                     (gcc-internal-include
                      (string-append gcc-lib
                       "/lib/gcc/x86_64-unknown-linux-gnu/14.3.0/include"))
                     (gcc-fixed-include
                      (string-append gcc-lib
                       "/lib/gcc/x86_64-unknown-linux-gnu/14.3.0/include-fixed")))
                ;; Remove the symlink that points to lib/svm/bin/native-image
                (when (file-exists? native-image-wrapper)
                  (delete-file native-image-wrapper))
                ;; Create wrapper script that sets up environment and runs
                ;; native-image directly (not via ld-linux)
                (call-with-output-file native-image-wrapper
                  (lambda (port)
                    (format port "#!~a~%" bash)
                    (format port "export PATH=\"~a/bin:~a/bin${PATH:+:}$PATH\"~%"
                            gcc-toolchain binutils)
                    (format port "export LIBRARY_PATH=\"~a/lib:~a/lib:~a/lib${LIBRARY_PATH:+:}$LIBRARY_PATH\"~%"
                            glibc zlib gcc-toolchain)
                    (format port "export C_INCLUDE_PATH=\"~a/include:~a/include:~a/include:~a/include${C_INCLUDE_PATH:+:}$C_INCLUDE_PATH\"~%"
                            glibc linux-headers zlib gcc-toolchain)
                    ;; LD_LIBRARY_PATH for runtime library loading
                    (format port "export LD_LIBRARY_PATH=\"~a/lib:~a/lib/server:~a/lib:~a/lib:~a/lib${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH\"~%"
                            out out glibc gcc-lib zlib)
                    ;; Pass include paths and library paths directly to native-image
                    ;; via -H: options since env vars may not be inherited by gcc
                    (format port "exec \"~a\" \\~%" native-image-bin)
                    (format port "  -H:+UnlockExperimentalVMOptions \\~%")
                    (format port "  \"-H:CCompilerOption=-I~a\" \\~%"
                            gcc-internal-include)
                    (format port "  \"-H:CCompilerOption=-I~a\" \\~%"
                            gcc-fixed-include)
                    (format port "  \"-H:CCompilerOption=-I~a/include\" \\~%"
                            glibc)
                    (format port "  \"-H:CCompilerOption=-I~a/include\" \\~%"
                            linux-headers)
                    (format port "  \"-H:CLibraryPath=~a/lib\" \\~%"
                            glibc)
                    (format port "  \"-H:CLibraryPath=~a/lib\" \\~%"
                            zlib)
                    (format port "  \"$@\"~%")))
                (chmod native-image-wrapper #o755))))
          ;; Disable dynamic linker cache - some GraalVM ELF files have
          ;; unusual format that confuses the cache generator
          (delete 'make-dynamic-linker-cache))))
    (inputs
     (list `(,gcc "lib")
           bash-minimal
           gcc-toolchain
           binutils
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
Edition of GraalVM based on OpenJDK 25.")
    (license license:gpl2+)))
