(define-module (imgui backends sdl2)
  #:export (impl:sdl2:init-opengl
            impl:sdl2:process-event
            impl:sdl2:shutdown
            impl:sdl2:new-frame))

(load-extension "libguile_dear_imgui_backend_sdl2.so" "init_imgui_sdl")
