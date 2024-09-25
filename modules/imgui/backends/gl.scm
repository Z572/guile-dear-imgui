(define-module (imgui backends gl)
  #:export (impl:opengl3:shutdown
            impl:opengl3:render-draw-data
            impl:opengl3:init
            impl:opengl3:new-frame))
(load-extension "libguile_dear_imgui_backend_gl" "init_imgui_gl")
