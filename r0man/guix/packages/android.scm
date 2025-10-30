(define-module (r0man guix packages android)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses))

(define-public android-sdk-platform-tools
  (package
    (name "android-sdk-platform-tools")
    (version "36.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://dl.google.com/android/repository/platform-tools-latest-linux.zip")
       (sha256
        (base32
         "1s7vrckwnv1zappfybnz8d7wxi393igqzjnc3xq7kzizjhn69b8f"))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip patchelf))
    (inputs
     (list glibc
           (list gcc "lib")
           ncurses
           zlib))
    (arguments
     (list
      #:validate-runpath? #f  ;; Pre-built binaries have complex dependencies
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "unzip" source)
              (chdir "platform-tools")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib"))
                     (libexec (string-append out "/libexec/android-sdk/platform-tools"))
                     (share (string-append out "/share/android-sdk/platform-tools")))
                ;; Install all files to libexec
                (mkdir-p libexec)
                (copy-recursively "." libexec)

                ;; Create bin directory and symlink executables
                (mkdir-p bin)
                (for-each
                 (lambda (exec)
                   (symlink (string-append libexec "/" exec)
                            (string-append bin "/" exec)))
                 '("adb" "fastboot" "etc1tool" "hprof-conv"
                   "make_f2fs" "make_f2fs_casefold" "mke2fs"))

                ;; Copy lib64 to lib
                (when (file-exists? (string-append libexec "/lib64"))
                  (copy-recursively (string-append libexec "/lib64") lib))

                ;; Install documentation
                (mkdir-p share)
                (when (file-exists? (string-append libexec "/NOTICE.txt"))
                  (copy-file (string-append libexec "/NOTICE.txt")
                             (string-append share "/NOTICE.txt")))
                (when (file-exists? (string-append libexec "/source.properties"))
                  (copy-file (string-append libexec "/source.properties")
                             (string-append share "/source.properties"))))))
          (add-after 'install 'patchelf
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (libexec (string-append out "/libexec/android-sdk/platform-tools"))
                     (glibc-dir (dirname (search-input-directory inputs "lib")))
                     (glibc-lib (string-append glibc-dir "/lib"))
                     (gcc-lib-dir
                      (dirname (dirname
                                (search-input-file inputs
                                                   "lib/libgcc_s.so.1"))))
                     (gcc-lib (string-append gcc-lib-dir "/lib"))
                     (ncurses-lib
                      (dirname (search-input-file inputs
                                                   "lib/libncurses.so")))
                     (zlib-lib (dirname (search-input-file inputs "lib/libz.so")))
                     (rpath (string-append glibc-lib ":" gcc-lib ":" ncurses-lib ":"
                                           zlib-lib ":" out "/lib")))
                ;; Patch all ELF executables
                (for-each
                 (lambda (file)
                   (let ((filepath (string-append libexec "/" file)))
                     (when (file-exists? filepath)
                       (invoke "patchelf" "--set-interpreter"
                               (string-append glibc-lib "/ld-linux-x86-64.so.2")
                               filepath)
                       (invoke "patchelf" "--set-rpath" rpath filepath))))
                 '("adb" "fastboot" "etc1tool" "hprof-conv"
                   "make_f2fs" "make_f2fs_casefold" "mke2fs" "sqlite3"))

                ;; Patch shared libraries in lib and lib64
                (when (directory-exists? (string-append out "/lib"))
                  (let ((lib-files '("libc++.so")))
                    (for-each
                     (lambda (lib-file)
                       (let ((filepath (string-append out "/lib/" lib-file)))
                         (when (file-exists? filepath)
                           (invoke "patchelf" "--set-rpath" rpath filepath))))
                     lib-files)))
                (when (directory-exists? (string-append libexec "/lib64"))
                  (let ((lib-files '("libc++.so")))
                    (for-each
                     (lambda (lib-file)
                       (let ((filepath (string-append libexec "/lib64/" lib-file)))
                         (when (file-exists? filepath)
                           (invoke "patchelf" "--set-rpath" rpath filepath))))
                     lib-files))))))
          (delete 'check))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://developer.android.com/tools/releases/platform-tools")
    (synopsis "Android SDK Platform-Tools")
    (description
     "Android SDK Platform-Tools is a component for the Android SDK that
includes tools that interface with the Android platform, primarily @command{adb}
and @command{fastboot}.  These tools are essential for Android development and
device management, allowing you to install and debug apps, access a Unix shell,
and flash system images to Android devices.")
    (license license:asl2.0)))
