(unless (false-if-exception(resolve-interface '(gl)))
  (exit 77))
(use-modules
 (imgui)
 (imgui backends glfw3)
 (glfw)
 (gl)
 (srfi srfi-71))

(define (do-new-frame)
  (impl:opengl3:new-frame)
  (impl:glfw3:new-frame))

(define (do-render)
  (set-gl-clear-color 0 0 0 0.5)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (impl:opengl3:render-draw-data))

(define (init w)
  (impl:glfw3:init-opengl
   (unwrap-window w)
   #t)
  (impl:opengl3:init))

(when (glfwinit)
  (glfwwindowhint GLFW_VISIBLE 0)
  (glfwwindowhint GLFW_CONTEXT_VERSION_MAJOR 3)
  (glfwwindowhint GLFW_CONTEXT_VERSION_MINOR 0)
  (let ((w (make-window
            #:size '(1280 720)
            #:title "GLFW+OpenGL3 example"))
        (input-i (make-parameter 0))
        (input-text-p (make-parameter "h"))
        (input-text-p2 (make-parameter ""))
        (slider-int-p (make-parameter 0))
        (drag-int-p (make-parameter 0))
        (color (make-parameter (list 0 0 0 0))))
    (make-context-current w)
    (set-swap-interval! 'vsync)
    (create-context)
    (init w)

    (while #f
      (poll-events)
      (do-new-frame)
      (new-frame)
      (with-window ("main" #f ImGuiWindowFlags_AlwaysAutoResize)
        (text-colored '(1 0 0 1) "R:"))

      (gl-viewport 0 0 100 100)
      (render)
      (do-render)
      (swap-buffers w))

    (impl:opengl3:shutdown)
    (impl:glfw3:shutdown)
    (destroy-context)
    (destroy-window w)
    (terminate)))
