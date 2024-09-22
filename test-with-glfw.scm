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
  (set-gl-clear-color 0 0 0 0.5)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (impl:opengl3:render-draw-data))

(define (init w)
  (impl:glfw:init-opengl
   (unwrap-window w)
   #t)
  (impl:opengl3:init))

(when (glfwinit)
  (glfwwindowhint GLFW_CONTEXT_VERSION_MAJOR 3)
  (glfwwindowhint GLFW_CONTEXT_VERSION_MINOR 0)
  (glfwwindowhint GLFW_TRANSPARENT_FRAMEBUFFER 1)
  (let ((w (make-window
            #:size '(1280 720)
            #:title "GLFW+OpenGL3 example"))
        (input-i (make-parameter 0))
        (slider-int-p (make-parameter 0))
        (drag-int-p (make-parameter 0)))
    (make-context-current w)
    (set-swap-interval! 'vsync)
    (create-context)
    (init w)

    (while (not (window-should-close? w))
      (poll-events)
      (do-new-frame)
      (new-frame)
      ;; (let ((n (io-display-size (get-io))))
      ;;   (set-next-window-size
      ;;    (car n)
      ;;    (cdr n)))
      ;; (set-next-window-pos 0 0 0 0 0)
      (with-window ("main" #f ImGuiWindowFlags_AlwaysAutoResize)
        (tab-bar ("bar1")
          (tab-item ("bar2")
            (text-colored '(1 0 0 1) "R:") (sameline)
            (input-int "hello!" input-i 1 100)
            (slider-int "slider" slider-int-p 1 100)
            (drag-int "drag" drag-int-p 1 0 100)
            (let ((status (gc-stats)))
              (for-each
               (lambda (x)
                 (text (string-append (symbol->string (car x))
                                      " -> "
                                      (number->string (cdr x)))))
               status)))
          (tab-item ("bar0")
            (progress-bar 0.5 (cons 0 0) "hov"))))

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
;; eval: (put 'tab-bar 'scheme-indent-function 1)
;; eval: (put 'tab-item 'scheme-indent-function 1)
;; eval: (put 'with-list-box 'scheme-indent-function 1)
;; eval: (put 'with-combo 'scheme-indent-function 1)
;; eval: (put 'with-ellipsis 'scheme-indent-function 1)
;; End:
