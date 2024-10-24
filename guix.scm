(use-modules
 (guix utils) (guix packages)
 ((guix licenses) #:prefix license:)
 (rnrs io ports)
 (gnu packages xorg)
 (guix download)
 (guix git-download)
 (gnu packages gettext)
 (guix gexp)
 (gnu packages gl)
 (gnu packages xdisorg)
 (guix build-system meson)
 (gnu packages bash)
 (gnu packages)
 (gnu packages  boost)
 (gnu packages autotools)
 (gnu packages cpp)
 (gnu packages guile)
 (gnu packages gtk)
 (gnu packages guile-xyz)
 (gnu packages ibus)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages wm)
 (gnu packages gdb)
 (gnu packages toolkits)
 (gnu packages stb)
 (gnu packages sdl)
 (gnu packages freedesktop))

(define %srcdir
  (dirname (current-filename)))

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
    (source (local-file "." "guile-dear-imgui"
                        #:recursive? #t
                        #:select? (git-predicate %srcdir)))
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
