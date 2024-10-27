(unless (false-if-exception
         (and (resolve-interface '(sdl2))
              (resolve-interface '(gl))))
  (exit 77))
(use-modules
 (srfi srfi-1)
 (imgui)
 ((imgui backends sdl2) #:prefix backend:sdl2:)
 ((imgui backends gl) #:prefix backend:opengl:)
 (sdl2)
 ((sdl2 bindings) #:prefix bind:)
 ((system foreign) #:prefix ffi:)
 (gl)
 (srfi srfi-71)
 (sdl2 render)
 (sdl2 surface)
 (imgui window)
 (sdl2 events)
 (sdl2 video))


(sdl-init)
(create-context)
(style-scale-all-sizes (get-style) 2)

(define s-window (make-window #:opengl? #t
                              #:resizable? #t
                              #:title "test"
                              #:high-dpi? #t))
(define s-context (make-gl-context s-window))

(set-gl-attribute! 'context-major-version 2)
(set-gl-attribute! 'context-minor-version 0)
(set-gl-attribute! 'context-flags 0)
(set-gl-attribute! 'context-profile-mask 2)
(set-gl-attribute! 'double-buffer 1)
(set-gl-attribute! 'depth-size 24)
(set-gl-attribute! 'stencil-size 8)
(set-gl-swap-interval! 'vsync)

(backend:sdl2:init-opengl
 ((@@ (sdl2 video) unwrap-window) s-window)
 ((@@ (sdl2 video) unwrap-gl-context) s-context))
(backend:opengl:init)





(define %sdl-event ((@@ (sdl2 events) make-sdl-event)))
(define sdl-event-ptr (ffi:bytevector->pointer %sdl-event))
(let loop ((event (bind:sdl-poll-event sdl-event-ptr)))
  (backend:sdl2:process-event sdl-event-ptr)
  (unless (= 1 event)
    (loop (bind:sdl-poll-event sdl-event-ptr))))
(backend:opengl:new-frame)
(backend:sdl2:new-frame)
(new-frame)
(let ((n (io-display-size (get-io))))
  (set-next-window-size! n))
(with-window ("aba"
              #:open? #f
              #:flags ImGuiWindowFlags_AlwaysAutoResize)
  (with-popup ("a-popup")
    (text "select1")
    (selectable "1" #f)
    (selectable "3" #f))
  (with-child-window ("bb")
    (with-list-box ("select")
      (selectable "2" #f)
      (selectable "2" #f)))
  (sameline)
  (group
   (indent)
   (text (format #f "font size: ~a" (get-font-size)))

   (sameline)
   (bullet)
   (sameline)
   (selectable "h" #f)
   (progress-bar 0.5 (cons 0 0) "hov"))
  (newline)
  (separator))

(render)

(let* ((w (io-display-size (get-io))))
  (gl-viewport 0 0 (inexact->exact (first w)) (inexact->exact (second w))))
(set-gl-clear-color 0 0 0 240)
(gl-clear (clear-buffer-mask color-buffer depth-buffer))
(backend:opengl:render-draw-data (draw-data))
(swap-gl-window s-window)


(backend:opengl:shutdown)
(backend:sdl2:shutdown)
(destroy-context)
(sdl-quit)
