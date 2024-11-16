(define-module (guile-dear-imgui-package)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (rnrs io ports)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages gettext)
  #:use-module (guix gexp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages)
  #:use-module (gnu packages  boost)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages toolkits)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages freedesktop))

(define %srcdir
  (string-append
   (current-source-directory)
   "/../.."))

(define-public imgui-1.91
  (package
    (inherit imgui)
    (name "imgui")
    (version "1.91.4")
    (arguments
     (substitute-keyword-arguments (package-arguments imgui)
       ((#:make-flags make-flags #~())
        #~(cons "-lfreetype" #$make-flags))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (replace 'build
              (lambda* (#:key make-flags #:allow-other-keys)
                ;; Build main library.
                (apply invoke #$(cxx-for-target)
                       (append make-flags
                               `("imgui.cpp"
                                 "imgui_draw.cpp"
                                 "imgui_tables.cpp"
                                 "imgui_demo.cpp"
                                 "imgui_widgets.cpp"
                                 ;; Include the supported backends.
                                 "backends/imgui_impl_glfw.cpp"
                                 ,(if (file-exists? "backends/imgui_impl_sdl2.cpp")
                                      "backends/imgui_impl_sdl2.cpp"
                                      "backends/imgui_impl_sdl.cpp")
                                 "backends/imgui_impl_opengl2.cpp"
                                 "backends/imgui_impl_opengl3.cpp"
                                 ;; Include wrappers for C++ standard library (STL) and
                                 ;; fontconfig.
                                 ,@(find-files "misc" "\\.cpp$"))))))))))
    (source (origin
              (inherit (package-source imgui))
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocornut/imgui")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lqcsyqj5m4d4g390x7n3jvjanrnsf64fgjxn51v1kc02dw28gpa"))))))

(define-public guile-dear-imgui
  (package
    (name "guile-dear-imgui")
    (version (call-with-input-file (string-append %srcdir "/" "meson.version")
               get-string-all))
    (source (local-file "../.." (git-file-name name version)
                        #:recursive? #t
                        #:select? (or (git-predicate %srcdir)
                                      (const #t))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config
           guile-3.0-latest))
    (inputs (list guile-3.0-latest imgui-1.91 sdl2 glfw-3.4
                  magic-enum
                  stb-image ;; optional dependency
                  ;; for example
                  guile-sdl2 guile3.0-opengl))
    (arguments
     (list
      #:configure-flags #~(list
                           "-DImDrawIdx=unsigned int"
                           (string-append
                            "-Dguile-extension-dir=lib"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unbundle-imgui
            (lambda _
              (substitute* "meson.build"
                (("dearimgui_dep = dependency\\('imgui',required: false\\)")
                 (string-append
                  "dearimgui_dep = declare_dependency(dependencies: "
                  "meson.get_compiler('cpp').find_library('imgui'), \
include_directories: '"
                  #$(this-package-input "imgui") "/include/imgui')")))))
          (add-after 'unpack 'set-extension-path
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* (find-files "." ".*\\.scm")
                (("\\(load-extension \"(.*)\" *\"(.*)\"\\)" _ letters o)
                 (string-append
                  (object->string
                   `(or (false-if-exception
                         (load-extension ,letters ,o))
                        (load-extension
                         ,(string-append
                           #$output
                           "/lib/" letters)
                         ,o))))))))
          (delete 'shrink-runpath))))

    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))
guile-dear-imgui
