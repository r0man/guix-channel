(define-module (r0man guix packages sqlite)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages sqlite)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

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
                "1jrrbx7i4dmkqs5m5390vy1sgsyvr4dhqkfswh0ryrh04nam9gvi"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "CMakeLists.txt"
                  ;; Don't use vendored faiss
                  (("add_subdirectory.*/vendor/faiss.*")
                   "")
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
                   "target_link_libraries(sqlite-vss-static PUBLIC faiss)")))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'configure-version
            (lambda* _
              (setenv "SQLITE_VSS_CMAKE_VERSION" #$version))))))
    (inputs (list faiss libomp-17 lapack nlohmann-json sqlite))
    (home-page "https://github.com/asg017/sqlite-vss")
    (synopsis "A SQLite extension for efficient vector search, based on Faiss!")
    (description "SQLite Vector Similarity Search is a SQLite extension that brings
vector search capabilities to SQLite, based on Faiss.  It can be used
to build semantic search engines, recommendations, or
questions-and-answering tools.")
    (license license:expat)))

;; libgomp1 libatlas-base-dev liblapack-dev libsqlite3-dev
