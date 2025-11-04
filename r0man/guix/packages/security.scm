(define-module (r0man guix packages security)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public cortex-agent
  (package
    (name "cortex-agent")
    (version "8.6.1.129181")
    (source
     (local-file "../../../cortex-86.rpm"))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("opt" "opt"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'extract-rpm
            (lambda* (#:key source outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                ;; Extract RPM using rpm2cpio and cpio
                (invoke "sh" "-c"
                        (string-append "rpm2cpio " #$source
                                       " | cpio -idmv"))
                ;; Extract nested tarball to opt/traps
                (with-directory-excursion "opt/traps/rpm-installer"
                  (invoke "tar" "-xzf"
                          (string-append "cortex-agent-"
                                         #$version
                                         ".tar.gz")
                          "-C" ".."
                          "--strip-components" "1"))
                ;; List files to verify extraction
                (system* "ls" "-la" "opt/traps/"))))
          (add-after 'extract-rpm 'patch-binaries
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (rpath (string-append
                            (assoc-ref outputs "out") "/opt/traps/glibc/lib:"
                            (assoc-ref %build-inputs "openssl") "/lib:"
                            (assoc-ref %build-inputs "glibc") "/lib")))
                ;; Patch ELF binaries to use correct interpreter and rpath
                (for-each
                 (lambda (binary)
                   (when (file-exists? binary)
                     (invoke "patchelf" "--set-interpreter"
                             (string-append
                              (assoc-ref %build-inputs "glibc")
                              "/lib/ld-linux-x86-64.so.2")
                             "--set-rpath" rpath
                             binary)))
                 '("opt/traps/bin/cytool"
                   "opt/traps/bin/pmd"
                   "opt/traps/bin/initd"
                   "opt/traps/analyzerd/sandboxd"
                   "opt/traps/analyzerd/spmd"))))))))
    (native-inputs
     (list cpio patchelf rpm))
    (inputs
     (list glibc openssl))
    (synopsis "Palo Alto Networks Cortex XDR endpoint security agent")
    (description
     "Cortex XDR is an endpoint security agent that provides comprehensive
protection including Anti-Virus/Anti-Malware, EDR (Endpoint Detection and
Response), Ransomware Protection, Exploit Prevention, and Malicious Behavioral
Prevention.  This package contains the agent for Linux systems.")
    (home-page "https://www.paloaltonetworks.com/cortex/cortex-xdr")
    (license #f))) ; Proprietary
