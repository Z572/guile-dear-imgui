#!/usr/bin/env -S guile
!#
(use-modules
 (system repl server)
 (imgui)
 ((imgui backends glfw3) #:prefix impl:glfw3:)
 ((imgui backends gl) #:prefix impl:opengl3:)
 (glfw)
 (gl)
 (srfi srfi-71))

(define (do-new-frame)
  (impl:opengl3:new-frame)
  (impl:glfw3:new-frame))

(define (do-render)
  (set-gl-clear-color 0 0 0 0.5)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (impl:opengl3:render-draw-data (draw-data)))

(define (init-glfw+opengl w)
  (impl:glfw3:init-opengl
   (unwrap-window w)
   #t)
  (impl:opengl3:init))

(define input-i (make-parameter 0))
(define input-text-p (make-parameter "h"))
(define input-text-p2 (make-parameter ""))
(define slider-int-p (make-parameter 0))
(define drag-int-p (make-parameter 0))
(define color (make-parameter (list 0 0 0 0)))
(define (draw!)
  (with-window ("main" #f ImGuiWindowFlags_AlwaysAutoResize)
    (tab-bar ("bar1")
      (tab-item ("bar2")
        (table ("t" 5)
               (table-next-column)
               (text (format #f "~a" (table-column-index)))
               (table-next-column)
               (text (format #f "~a" (table-column-index)))
               (when (table-next-column)
                 (text (format #f "kk: ~a" (table-column-index))))
               (table-next-column)
               (text (format #f "~a" (table-column-index)))
               (table-next-column)
               (text (format #f "~a" (table-column-index)))
               (table-next-row)
               (table-next-column)
               (text "askfd")
               (table-next-column)
               (text "ba")
               (table-next-row)
               (table-next-column)
               (text "b")
               (table-next-row)
               (table-next-column)
               (text "c"))
        (show-style-selector "hello")
        (text-colored '(1 0 0 1) "R:") (sameline)
        (input-int "hello!" input-i 1 100)
        (text (string-append "output :"( input-text-p)))
        (input-text "text" input-text-p)
        (input-text "text2" input-text-p2
                    #:hint "dfsdf")
        (default-focus)
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
        (color-picker4 "color:" color)
        (progress-bar 0.5 (cons 0 0) "hov")))))
(when (init)
  (window-hint GLFW_CONTEXT_VERSION_MAJOR 3)
  (window-hint GLFW_CONTEXT_VERSION_MINOR 0)
  (window-hint GLFW_TRANSPARENT_FRAMEBUFFER 1)
  (let ((w (make-window
            #:size '(1280 720)
            #:title "GLFW+OpenGL3 example")))
    (make-context-current w)
    (set-swap-interval! 'vsync)
    (create-context)
    (init-glfw+opengl w)
    (spawn-server)
    (while (not (window-should-close? w))
      (poll-events)
      (do-new-frame)
      (new-frame)
      ;; (let ((n (io-display-size (get-io))))
      ;;   (set-next-window-size
      ;;    (car n)
      ;;    (cdr n)))
      ;; (set-next-window-pos 0 0 0 0 0)
      (draw!)

      (gl-viewport 0 0 100 100)
      (render)
      (do-render)
      (swap-buffers w))

    (impl:opengl3:shutdown)
    (impl:glfw3:shutdown)
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
