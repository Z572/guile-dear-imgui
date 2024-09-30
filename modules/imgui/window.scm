(define-module (imgui window)
  #:use-module (srfi srfi-1)
  #:export (window-appearing?
            window-focused?
            window-hovered?
            window-collapsed?
            set-next-window-size!
            set-next-window-position!))

(load-extension "libguile_dear_imgui.so" "init_imgui_window")

(define set-next-window-size!
  (case-lambda ((x) (%set-next-window-size! (first x) (second x)))
               ((x y) (%set-next-window-size! x y))))
