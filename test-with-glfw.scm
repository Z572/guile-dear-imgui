#!/usr/bin/env -S guile -L modules
!#
(use-modules
 (imgui)
 (glfw)
 (gl)
 (srfi srfi-71))

(define (do-new-frame)
  (impl:opengl3:new-frame)
  (impl:glfw:new-frame))

(define (do-render)
  (set-gl-clear-color 0 0 0 240)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (impl:opengl3:render-draw-data)
  )

(define (init w)
  (impl:glfw:init-opengl
   (unwrap-window w)
   #t)
  (impl:opengl3:init))

(when (glfwinit)
  (glfwwindowhint GLFW_CONTEXT_VERSION_MAJOR 3)
  (glfwwindowhint GLFW_CONTEXT_VERSION_MINOR 0)
  (let ((w (make-window
            #:size '(1280 720)
            #:title "GLFW+OpenGL3 example"))
        (input-i (make-parameter 0)))
    (make-context-current w)
    (set-swap-interval! 'vsync)
    (create-context)
    (init w)

    (while (not (window-should-close? w))
      (poll-events)
      (do-new-frame)
      (new-frame)


      (with-window ("main" #f ImGuiWindowFlags_AlwaysAutoResize)
        (input-int "hello!" input-i 1 100))

      (gl-viewport 0 0 100 100)
      (render)
      (do-render)
      (swap-buffers w))

    (impl:opengl3:shutdown)
    (impl:glfw:shutdown)
    (destroy-context)
    (destroy-window w)
    (terminate)))



;; Local Variables:
;; eval: (put 'with-window 'scheme-indent-function 1)
;; eval: (put 'with-child-window 'scheme-indent-function 1)
;; eval: (put 'with-menu 'scheme-indent-function 1)
;; eval: (put 'with-popup 'scheme-indent-function 1)
;; eval: (put 'with-list-box 'scheme-indent-function 1)
;; eval: (put 'with-combo 'scheme-indent-function 1)
;; End:
