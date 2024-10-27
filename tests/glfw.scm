(unless (false-if-exception (resolve-interface '(gl)))
  (exit 77))
(use-modules
 (imgui)
 ((imgui backends glfw3) #:prefix backend:glfw3:)
 ((imgui backends gl) #:prefix backend:gl:)
 (glfw)
 (gl)
 (srfi srfi-71))

(define (do-new-frame)
  (backend:gl:new-frame)
  (backend:glfw3:new-frame))

(define (do-render)
  (set-gl-clear-color 0 0 0 0.5)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (backend:gl:render-draw-data (draw-data)))
(define (init-glfw+gl w)
  (backend:glfw3:init-opengl
   (unwrap-window w)
   #t)
  (backend:gl:init))

(when (init)
  (window-hint GLFW_VISIBLE 0)
  (window-hint GLFW_CONTEXT_VERSION_MAJOR 3)
  (window-hint GLFW_CONTEXT_VERSION_MINOR 0)
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
    (init-glfw+gl w)

    (begin
      (poll-events)
      (do-new-frame)
      (new-frame)
      (with-window ("main" #:flags ImGuiWindowFlags_AlwaysAutoResize)
        (text-colored '(1 0 0 1) "R:"))

      (gl-viewport 0 0 100 100)
      (render)
      (do-render)
      (swap-buffers w))

    (backend:gl:shutdown)
    (backend:glfw3:shutdown)
    (destroy-context)
    (destroy-window w)
    (terminate)))
