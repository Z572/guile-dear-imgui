(define-module (imgui backends sdl2)
  #:export (init-opengl
            process-event
            shutdown
            new-frame))

(load-extension "libguile_dear_imgui_backend_sdl2.so" "init_imgui_sdl")
