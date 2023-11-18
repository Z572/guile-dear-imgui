(use-modules
 (guix utils) (guix packages)
 ((guix licenses) #:prefix license:)
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
 (gnu packages guile)
 (gnu packages gtk)
 (gnu packages guile-xyz)
 (gnu packages ibus)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages wm)
 (gnu packages gdb)
 (gnu packages toolkits)
 (gnu packages freedesktop))

(define %srcdir
  (dirname (current-filename)))

(define-public guile-dear-imgui
  (package
    (name "guile-dear-imgui")
    (version "0")
    (source (local-file "." "guile-dear-imgui"
                        #:recursive? #t
                        #:select? (git-predicate %srcdir)))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config
           guile-3.0-latest))
    (inputs (list guile-3.0-latest imgui schmutz boost))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))
guile-dear-imgui
