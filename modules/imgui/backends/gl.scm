(define-module (imgui backends gl)
  #:export (shutdown
            render-draw-data
            init
            new-frame
            load-image))

(define-once load-image
  (lambda (filename)
    (error "stb-image.h not found on compiletime! function not available.")))

(load-extension "libguile_dear_imgui_backend_gl" "init_imgui_gl")
