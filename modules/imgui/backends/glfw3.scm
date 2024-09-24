(define-module (imgui backends glfw3)
  #:export (impl:glfw3:new-frame
            impl:glfw3:init-opengl
            impl:glfw3:shutdown))


(load-extension "libguile_dear_imgui_backend_glfw3" "init_imgui_glfw")
