(define-module (imgui window)
  #:export (window-appearing?
            window-focused?
            window-hovered?
            window-collapsed?
            set-next-window-size!
            set-next-window-position!))

(load-extension "libguile_dear_imgui.so" "init_imgui_window")
