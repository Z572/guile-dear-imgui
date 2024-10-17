(define-module (imgui backends glfw3)
  #:export (new-frame
            init-opengl
            shutdown))


(load-extension "libguile_dear_imgui_backend_glfw3" "init_imgui_glfw")
