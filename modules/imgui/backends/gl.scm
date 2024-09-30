(define-module (imgui backends gl)
  #:export (impl:opengl3:shutdown
            impl:opengl3:render-draw-data
            impl:opengl3:init
            impl:opengl3:new-frame
            load-image))

(define-once load-image
  (lambda (filename)
    (error "stb-image.h not found on compiletime! function not available.")))

(load-extension "libguile_dear_imgui_backend_gl" "init_imgui_gl")
