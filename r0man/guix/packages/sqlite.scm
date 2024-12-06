(define-module (r0man guix packages sqlite)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages check)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages sqlite)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public faiss-1.9
  (package
    (name "faiss")
    (version "1.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/facebookresearch/faiss")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0920lfqxrl7ck2dvvcqa7mvlczidz2wj7pb2f5dr7kx39sfz5i1z"))
              (modules '((guix build utils)))
              ;; (snippet
              ;;  '(begin
              ;;     (substitute* "utils.cpp"
              ;;       (("#include <immintrin.h>")
              ;;        "#ifdef __SSE__\n#include <immintrin.h>\n#endif"))
              ;;     #t))
              ))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DFAISS_ENABLE_GPU=OFF" ; thanks, but no thanks, CUDA.
             "-DFAISS_ENABLE_PYTHON=OFF"
             "-DBUILD_TESTING=OFF")    ; we don't need those
       #:phases
       (modify-phases %standard-phases
         ;; (add-after 'unpack 'prepare-build
         ;;            (lambda _
         ;;              (let ((features (list ,@(let ((system (or (%current-target-system)
         ;;                                                        (%current-system))))
         ;;                                        (cond
         ;;                                         ((string-prefix? "x86_64" system)
         ;;                                          '("-mavx" "-msse2" "-mpopcnt"))
         ;;                                         ((string-prefix? "i686" system)
         ;;                                          '("-msse2" "-mpopcnt"))
         ;;                                         (else
         ;;                                          '()))))))
         ;;                (substitute* "CMakeLists.txt"
         ;;                  (("-m64") "")
         ;;                  (("-mpopcnt") "")      ; only some architectures
         ;;                  (("-msse4")
         ;;                   (string-append
         ;;                    (string-join features)
         ;;                    " -I" (getcwd)))
         ;;                  ;; Build also the shared library
         ;;                  (("ARCHIVE DESTINATION lib")
         ;;                   "LIBRARY DESTINATION lib")
         ;;                  (("add_library.*" m)
         ;;                   "\
         ;; add_library(objlib OBJECT ${faiss_cpu_headers} ${faiss_cpu_cpp})
         ;; set_property(TARGET objlib PROPERTY POSITION_INDEPENDENT_CODE 1)
         ;; add_library(${faiss_lib}_static STATIC $<TARGET_OBJECTS:objlib>)
         ;; add_library(${faiss_lib} SHARED $<TARGET_OBJECTS:objlib>)
         ;; install(TARGETS ${faiss_lib}_static ARCHIVE DESTINATION lib)
         ;; \n")))

         ;;              ;; See https://github.com/facebookresearch/faiss/issues/520
         ;;              ;; (substitute* "IndexScalarQuantizer.cpp"
         ;;              ;;   (("#define USE_AVX") ""))

         ;;              ;; Make header files available for compiling tests.
         ;;              (mkdir-p "faiss")
         ;;              (for-each (lambda (file)
         ;;                          (mkdir-p (string-append "faiss/" (dirname file)))
         ;;                          (copy-file file (string-append "faiss/" file)))
         ;;                        (find-files "." "\\.h$"))
         ;;              #t))
         (delete 'check)
         ;; (replace 'check
         ;;   (lambda _
         ;;     (invoke "make" "-C" "tests"
         ;;             (format #f "-j~a" (parallel-job-count)))))
         ;; (add-after 'install 'remove-tests
         ;;   (lambda* (#:key outputs #:allow-other-keys)
         ;;     (delete-file-recursively
         ;;      (string-append (assoc-ref outputs "out")
         ;;                     "/test"))
         ;;     #t))
         )))
    (inputs
     (list openblas))
    (native-inputs
     (list googletest))
    (home-page "https://github.com/facebookresearch/faiss")
    (synopsis "Efficient similarity search and clustering of dense vectors")
    (description "Faiss is a library for efficient similarity search and
clustering of dense vectors.  It contains algorithms that search in sets of
vectors of any size, up to ones that possibly do not fit in RAM.  It also
contains supporting code for evaluation and parameter tuning.")
    (license license:bsd-3)))

(define-public sqlite-vss
  (package
    (name "sqlite-vss")
    (version "0.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/asg017/sqlite-vss")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jrrbx7i4dmkqs5m5390vy1sgsyvr4dhqkfswh0ryrh04nam9gvi"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-files
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "CMakeLists.txt"
                ;; Don't use vendored faiss
                (("add_subdirectory.*/vendor/faiss.*")
                 (string-append "find_package(faiss REQUIRED)\n"
                                "find_package(OpenMP REQUIRED)"))
                ;; Don't use vendored sqlite
                (("include_directories\\(vendor/sqlite\\)") "")
                (("link_directories\\(BEFORE vendor/sqlite\\)") "")
                ;; Don't use vendored nlohmann_json
                (("add_subdirectory\\(vendor/json\\)")
                 "find_package(nlohmann_json REQUIRED)")
                ;; Use faiss instead of faiss_avx2 for sqlite-vss
                (("target_link_libraries\\(sqlite-vss faiss_avx2\\)")
                 "target_link_libraries(sqlite-vss faiss)")
                ;; Use faiss instead of faiss_avx2 for sqlite-vss-static
                (("target_link_libraries\\(sqlite-vss-static PUBLIC faiss_avx2\\)")
                 "target_link_libraries(sqlite-vss-static PUBLIC faiss)"))
              (substitute* "src/sqlite-vss.cpp"
                (("#include <faiss/impl/io.h>")
                 (string-append "#include <faiss/impl/io.h>\n"
                                "#include <faiss/impl/FaissException.h>\n")))))
          (add-before 'configure 'configure-version
            (lambda* _
              (setenv "SQLITE_VSS_CMAKE_VERSION" #$version)))
          (delete 'check)
          (replace 'install
            (lambda _
              (let ((lib (string-append #$output "/lib")))
                (mkdir-p lib)
                (for-each (lambda (file)
                            (install-file file lib))
                          '("libsqlite_vector0.a"
                            "libsqlite_vss0.a"
                            "vector0.so"
                            "vss0.so"))))))))
    (inputs (list faiss-1.9 libomp-17 lapack nlohmann-json sqlite openblas))
    (home-page "https://github.com/asg017/sqlite-vss")
    (synopsis "A SQLite extension for efficient vector search, based on Faiss!")
    (description "SQLite Vector Similarity Search is a SQLite extension that brings
vector search capabilities to SQLite, based on Faiss.  It can be used
to build semantic search engines, recommendations, or
questions-and-answering tools.")
    (license license:expat)))
