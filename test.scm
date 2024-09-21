(use-modules (sdl2)
             ((sdl2 bindings) #:prefix bind:)
             ((system foreign) #:prefix ffi:)
             (gl)
             (srfi srfi-71)
             (sdl2 render)
             (sdl2 surface)
             (sdl2 events)
             (sdl2 video))

(load-extension "build/libguile_dear_imgui.so" "init_imgui")
(sdl-init)
(define context (pk 'context (imgui:create-context)))
(define io (imgui:get-io))
(define style (imgui:get-style))
(imgui:style-scaleallsizes style 2)
(imgui:setup-font io)
(set-gl-attribute! 'context-major-version 2)
(set-gl-attribute! 'context-minor-version 0)
(set-gl-attribute! 'context-flags 0)
(set-gl-attribute! 'context-profile-mask 2)
(set-gl-attribute! 'double-buffer 1)
(set-gl-attribute! 'depth-size 24)
(set-gl-attribute! 'stencil-size 8)
(define s-window (make-window #:opengl? #t
                              #:title "demo"
                              #:high-dpi? #t))
(define s-context (make-gl-context s-window))
                                        ;(gl-context-make-current! s-window s-context)
(set-gl-swap-interval! 'vsync)
;; (call-with-window (make-window)
;;   (lambda (window)
;;     (call-with-renderer (make-renderer window) draw)))

(imgui:impl:sdl2:init-opengl ((@@ (sdl2 video) unwrap-window) s-window)
                             ((@@ (sdl2 video) unwrap-gl-context) s-context))
(imgui:impl:opengl3:init)
(define done? #f)

(define %sdl-event ((@@ (sdl2 events) make-sdl-event)))
(define sdl-event-ptr (ffi:bytevector->pointer %sdl-event))

(define-syntax-rule (begin-window name body ...)
  (begin (when (imgui:begin name)
           body ...)
         (imgui:end)))
(define-syntax-rule (imgui:tooltip body ...)
  (when (imgui:begin-tooltip)
    body ...
    (imgui:end-tooltip)))
(define-syntax-rule (imgui:item-tooltip body ...)
  (when (imgui:begin-item-tooltip)
    body ...
    (imgui:end-tooltip)))

(define checkbox-checked? #f)
(define input-n 0)
(while (not done?)
  (let loop ((event (bind:sdl-poll-event sdl-event-ptr)))
    ;; (when event (pk 'event event))
    ;; (when event
    ;;   (when (or (quit-event? event) (window-closed-event? event))
    ;;     (set! done? #t))
    ;;   (loop (poll-event))
    ;;   )
    (imgui:impl:sdl2:process-event sdl-event-ptr)
    (unless (= 1 event)
      (loop (bind:sdl-poll-event sdl-event-ptr))))
  (imgui:impl:opengl3:new-frame)
  (imgui:impl:sdl2:new-frame)
  (imgui:new-frame)
  (let ((n (imgui:io-display-size io)))
    (imgui:set-next-window-size
     (car n)
     (cdr n)
     ;; 1000 200
     ))
  (imgui:set-next-window-pos 0 0 0 0 0)
  (when (and #f (imgui:begin-main-menu-bar))
    (when (imgui:menu-item "hello" "ba: a" #f #t)
      (pk 'clock)
      (set! done? #t))
    (imgui:end-main-menu-bar))

  (begin-window "aba"
    (imgui:text "hello")

    (imgui:sameline)
    (imgui:text (imgui:get-version))
    (imgui:begin-group)
    (let ((cliceed state (imgui:checkbox "check heerer!" checkbox-checked?))
          (cliceed-i state2 (imgui:input-int "input int:" input-n 1 100)))
      (imgui:button "hello:")
      (when cliceed
        (set! checkbox-checked? state))
      (when cliceed-i
        (set! input-n state2))
      (when checkbox-checked?
        (imgui:sameline)
        (imgui:text "world")))
    (imgui:end-group)
    (when checkbox-checked?
      (imgui:item-tooltip
       (imgui:text "tooltip"))
      )


    (when (imgui:textlink "https//a不过")
      (pk 'clock)
      (set! done? #t))
    (imgui:separator)
    (imgui:newline))

  (imgui:render)

  (let* ((w (imgui:io-display-size io)))
    (gl-viewport 0 0 (car w) (cdr w)))
  (set-gl-clear-color 0 0 0 240)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (imgui:impl:opengl3:render-draw-data)
  (swap-gl-window s-window))


(imgui:impl:opengl3:shutdown)
(imgui:impl:sdl2:shutdown)
(sdl-quit)

;; Local Variables:
;; eval: (put 'begin-window 'scheme-indent-function 1)
;; End:
