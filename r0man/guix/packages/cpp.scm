(define-module (r0man guix packages cpp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages llvm)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public c2ffi-13
  (let ((commit "bfa50485ffa86b886215c72ea1e43dbd3acaf940"))
    (package
      (name "c2ffi-13")
      ;; As per the c2ffi README: the first three elements are encoding the
      ;; required Clang/LLVM version, and the last one is the c2ffi revision.
      (version "13.0.0.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpav/c2ffi")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1r44qa5gz1xpyn0xi4s9nd2hqvf5i0gmb2nq70jbb3n7yv16390p"))
         (modules '((guix build utils)))
         (snippet
          '(substitute* "CMakeLists.txt"
             ;; Guix seems to be packaging LLVM libs separately thus -lLLVM
             ;; won't work, every used library must be specified explicitly.
             (("c2ffi PUBLIC clang-cpp LLVM")
              "c2ffi PUBLIC clang-cpp LLVMCore LLVMSupport LLVMMCParser \
LLVMOption LLVMBitReader LLVMProfileData")))))
      (build-system cmake-build-system)
      (arguments
       '(;; If LLVM was built without RTTI, we need to also be built without
         ;; it.  See: https://stackoverflow.com/q/11904519
         #:configure-flags '("-DCMAKE_CXX_FLAGS=-fno-rtti")
                           #:phases
                           (modify-phases %standard-phases
                             (replace 'check
                               (lambda* (#:key tests? #:allow-other-keys)
                                 (when tests?
                                   (invoke "./bin/c2ffi" "--help")))))))
      (native-inputs
       (list clang-13)) ; CMakeLists.txt invokes `clang -print-resource-dir`
      (inputs
       (list clang-13)) ; Compiled with gcc, but links against libclang-cpp.so
      (home-page "https://github.com/rpav/c2ffi")
      (synopsis "Clang-based FFI wrapper generator")
      (description
       "@code{c2ffi} is a tool for extracting definitions from C, C++, and
Objective C headers for use with foreign function call interfaces.  It uses
the @code{Clang/LLVM} infrastructure to extract the data, and emits it in
various formats, including @code{json}.")
      (license license:gpl2+))))
