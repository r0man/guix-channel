;;; GNU Guix --- Functional package management for GNU
;;; Copyright (C) 2024 Roman Scherer <roman@burningswell.com>
;;;
;;; This file is part of guix-channel-cortex.
;;;
;;; guix-channel-cortex is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; guix-channel-cortex is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guix-channel-cortex.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (r0man guix packages cortex)
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

;;; Commentary:
;;;
;;; Palo Alto Networks Cortex XDR Agent package for Guix System.
;;;
;;; Cortex XDR provides comprehensive endpoint security including:
;;; - Anti-Virus/Anti-Malware protection
;;; - EDR (Endpoint Detection and Response)
;;; - Ransomware Protection
;;; - Exploit Prevention
;;; - Behavioral Threat Protection
;;;
;;; System Requirements:
;;; - Architecture: x86_64 only
;;; - Kernel: Linux 2.6.32+ (3.4+ recommended for full functionality)
;;; - RAM: 4GB minimum (8GB recommended)
;;; - Disk: 10GB available space
;;; - Network: HTTPS access to Cortex XDR management server
;;; - License: Valid Palo Alto Networks Cortex XDR license
;;;
;;; Installation:
;;;
;;; Add this channel to ~/.config/guix/channels.scm:
;;;
;;;   (cons* (channel
;;;           (name 'guix-channel-cortex)
;;;           (url "https://github.com/r0man/guix-channel-cortex")
;;;           (branch "main"))
;;;          %default-channels)
;;;
;;; Then run: guix pull
;;;
;;; To build the package:
;;;
;;;   guix build -L . cortex-agent
;;;
;;; For service configuration, see (r0man guix services cortex).
;;;
;;; Known Limitations:
;;;
;;; 1. Kernel Module Compatibility
;;;    The kernel module requires a compatible kernel version.  If you
;;;    experience issues, disable it in the service configuration with
;;;    (kernel-module? #f).  This disables behavioral threat protection.
;;;
;;; 2. Proprietary Software
;;;    The Cortex XDR Agent is proprietary software from Palo Alto Networks.
;;;    This package repackages the official RPM distribution for Guix System.
;;;    You must have a valid license to use the agent.
;;;
;;; Code:

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
              (let* ((out (assoc-ref outputs "out"))
                     (rpath (string-append
                             out "/opt/traps/lib:"
                             out "/opt/traps/glibc/lib:"
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

;;; cortex.scm ends here
