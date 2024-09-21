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
(set-gl-swap-interval! 'vsync)

(imgui:impl:sdl2:init-opengl
 ((@@ (sdl2 video) unwrap-window) s-window)
 ((@@ (sdl2 video) unwrap-gl-context) s-context))
(imgui:impl:opengl3:init)
(define done? #f)

(define %sdl-event ((@@ (sdl2 events) make-sdl-event)))
(define sdl-event-ptr (ffi:bytevector->pointer %sdl-event))

(define-syntax-rule (with-window name body ...)
  (begin (when (imgui:begin name)
           body ...)
         (imgui:end)))

(define-syntax-rule (group body ...)
  (begin (imgui:begin-group)
         body  ...
         (imgui:end-group)))

(define-syntax-rule (imgui:tooltip body ...)
  (when (imgui:begin-tooltip)
    body ...
    (imgui:end-tooltip)))
(define-syntax-rule (with-menu (label act) body ...)
  (when (imgui:begin-menu label act)
    body ...
    (imgui:end-menu)))
(define-syntax-rule (with-popup (label) body ...)
  (when (imgui:begin-popup label)
    body ...
    (imgui:end-popup)))
(define-syntax-rule (imgui:item-tooltip body ...)
  (when (imgui:begin-item-tooltip)
    body ...
    (imgui:end-tooltip)))

(define checkbox-checked? #f)
(define input-n 0)
(define popup-select-1 #f)
(while (not done?)
  (let loop ((e (bind:sdl-poll-event sdl-event-ptr)))
    (imgui:impl:sdl2:process-event sdl-event-ptr)
    (SDL_PeepEvents sdl-event-ptr)
    (let ((event (poll-event)))
      (when event
        (pk 'event event)
        (when (or (quit-event? event) (window-closed-event? event))
          (set! done? #t))
        (loop (bind:sdl-poll-event sdl-event-ptr)))))
  (imgui:impl:opengl3:new-frame)
  (imgui:impl:sdl2:new-frame)
  (imgui:new-frame)
  (let ((n (imgui:io-display-size io)))
    (imgui:set-next-window-size
     (car n)
     (cdr n)))
  (imgui:set-next-window-pos 0 0 0 0 0)
  (when (and checkbox-checked?
             (imgui:begin-main-menu-bar))
    (with-menu ("hh" #t)
      (imgui:menu-item "bf" "Ctrl+N" #t #t)
      (imgui:menu-item "2" "bb" #f #t)
      (with-menu ("ba" #t)
        (imgui:menu-item "2" "bb" #f #t)))
    (when (imgui:menu-item "hello" "Ctrl+N" #f #t)
      (pk 'clock)
      (set! done? #t))
    (imgui:end-main-menu-bar))

  (with-window "aba"
    (with-popup ("a-popup")
      (imgui:text "select1")
      (imgui:selectable "2" #f)
      (when (imgui:selectable "3" popup-select-1)
        (set! popup-select-1 (not popup-select-1))))
    (imgui:indent)
    (imgui:text "hello")

    (imgui:sameline)
    (imgui:bullet)
    (imgui:text (imgui:get-version))
    (imgui:unindent)
    (group
     (let ((cliceed state (imgui:checkbox "check heerer!" checkbox-checked?))
           (cliceed-i state2 (imgui:input-int "input int:" input-n 1 100)))
       (when (imgui:button "hello:")
         (imgui:open-popup "a-popup"))
       (when cliceed
         (set! checkbox-checked? state))
       (when cliceed-i
         (set! input-n state2))
       (when checkbox-checked?
         (imgui:sameline 0 50)
         (imgui:text "world"))))
    (imgui:small-button ">")
    (imgui:sameline)
    (imgui:selectable "h" #f)
    (when checkbox-checked?
      (imgui:item-tooltip
       (imgui:text "tooltip")))

    (when (imgui:textlink "https//a不过")
      (pk 'clock)
      (set! done? #t))
    (imgui:textlink-open-url "author" "https://github.com/z572")
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
;; eval: (put 'with-window 'scheme-indent-function 1)
;; eval: (put 'with-menu 'scheme-indent-function 1)
;; eval: (put 'with-popup 'scheme-indent-function 1)
;; End:
