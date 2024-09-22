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
(define context (pk 'context (create-context)))
(define io (get-io))
(define style (get-style))
(style-scaleallsizes style 2)
(setup-font io)
(set-gl-attribute! 'context-major-version 2)
(set-gl-attribute! 'context-minor-version 0)
(set-gl-attribute! 'context-flags 0)
(set-gl-attribute! 'context-profile-mask 2)
(set-gl-attribute! 'double-buffer 1)
(set-gl-attribute! 'depth-size 24)
(set-gl-attribute! 'stencil-size 8)
(define s-window (make-window #:opengl? #t
                              #:resizable? #t
                              #:title "demo"
                              #:high-dpi? #t))
(define s-context (make-gl-context s-window))
                                        ;(gl-context-make-current! s-window s-context)
(set-gl-swap-interval! 'vsync)
;; (call-with-window (make-window)
;;   (lambda (window)
;;     (call-with-renderer (make-renderer window) draw)))

(impl:sdl2:init-opengl
 ((@@ (sdl2 video) unwrap-window) s-window)
 ((@@ (sdl2 video) unwrap-gl-context) s-context))
(impl:opengl3:init)
(define done? #f)

(define %sdl-event ((@@ (sdl2 events) make-sdl-event)))
(define sdl-event-ptr (ffi:bytevector->pointer %sdl-event))

(define-syntax-rule (with-window name body ...)
  (begin (when (begin-window name)
           body ...)
         (end-window)))
(define-syntax-rule (with-child-window name body ...)
  (begin (when (begin-child name)
           body ...)
         (end-child)))

(define-syntax-rule (with-list-box (name x y) body ...)
  (when (begin-list-box name x y)
    body ...
    (end-list-box)))

(define-syntax-rule (group body ...)
  (begin (begin-group)
         body  ...
         (end-group)))

(define-syntax-rule (tooltip body ...)
  (when (begin-tooltip)
    body ...
    (end-tooltip)))
(define-syntax-rule (with-menu (label act) body ...)
  (when (begin-menu label act)
    body ...
    (end-menu)))
(define-syntax-rule (with-popup (label) body ...)
  (when (begin-popup label)
    body ...
    (end-popup)))
(define-syntax-rule (item-tooltip body ...)
  (when (begin-item-tooltip)
    body ...
    (end-tooltip)))

(define checkbox-checked? #f)
(define input-n 0)
(define input-2 0.0)
(define popup-select-1 #f)
(while (not done?)
  (let loop ((event (bind:sdl-poll-event sdl-event-ptr)))
    ;; (when event (pk 'event event))
    ;; (when event
    ;;   (when (or (quit-event? event) (window-closed-event? event))
    ;;     (set! done? #t))
    ;;   (loop (poll-event))
    ;;   )
    (impl:sdl2:process-event sdl-event-ptr)
    (unless (= 1 event)
      (loop (bind:sdl-poll-event sdl-event-ptr))))
  (impl:opengl3:new-frame)
  (impl:sdl2:new-frame)
  (new-frame)
  (let ((n (io-display-size io)))
    (set-next-window-size
     (car n)
     (cdr n)
     ;; 1000 200
     ))
  (set-next-window-pos 0 0 0 0 0)
  (when (and checkbox-checked?
             (begin-main-menu-bar))
    (with-menu ("hh" #t)
      (menu-item "bf" "Ctrl+N" #t #t)
      (menu-item "2" "bb" #f #t)
      (with-menu ("ba" #t)
        (menu-item "2" "bb" #f #t)))
    (when (menu-item "hello" "Ctrl+N" #f #t)
      (pk 'clock)
      (set! done? #t))
    (end-main-menu-bar))

  (with-window "aba"
    (with-popup ("a-popup")
      (text "select1")
      (selectable "2" #f)
      (when (selectable "3" popup-select-1)
        (set! popup-select-1 (not popup-select-1))))
    (with-child-window "bb"
      (with-list-box ("select" 50 0)
        (selectable "2" #f)
        (selectable "2" #f)
        (selectable "2" #f)))
    (sameline)
    (group
     (indent)
     (text (format #f "font size: ~a" (get-font-size)))

     (sameline)
     (bullet)
     (text (get-version))
     (unindent)
     (let ((cliceed state (checkbox "check heerer!" checkbox-checked?))
           (cliceed-i state2 (input-int (format #f "value is ~a" input-n) input-n 1 100))
           (cliceed-2 state3 (input-float (format #f "value is ~a" input-2) input-2 20 100)))
       (when (button "hello:")
         (open-popup "a-popup"))
       (when cliceed
         (set! checkbox-checked? state))
       (when cliceed-i
         (set! input-n state2))
       (when cliceed-2
         (set! input-2 state3))
       (when checkbox-checked?
         (sameline 0 50)
         (text "world")))
     (small-button ">")
     (sameline)
     (selectable "h" #f)
     (when checkbox-checked?
       (item-tooltip
        (text "tooltip")))

     (when (textlink "https//a不过")
       (pk 'clock)
       (set! done? #t))
     (textlink-open-url "author" "https://github.com/z572"))

    (separator)
    (newline))

  (render)

  (let* ((w (io-display-size io)))
    (gl-viewport 0 0 (car w) (cdr w)))
  (set-gl-clear-color 0 0 0 240)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (impl:opengl3:render-draw-data)
  (swap-gl-window s-window))


(impl:opengl3:shutdown)
(impl:sdl2:shutdown)
(sdl-quit)

;; Local Variables:
;; eval: (put 'with-window 'scheme-indent-function 1)
;; eval: (put 'with-child-window 'scheme-indent-function 1)
;; eval: (put 'with-menu 'scheme-indent-function 1)
;; eval: (put 'with-popup 'scheme-indent-function 1)
;; eval: (put 'with-list-box 'scheme-indent-function 1)
;; End:
