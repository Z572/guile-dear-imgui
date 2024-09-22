#!/usr/bin/env -S guile -L modules
!#
(use-modules
 (imgui)
 (sdl2)
 ((sdl2 bindings) #:prefix bind:)
 ((system foreign) #:prefix ffi:)
 (gl)
 (srfi srfi-71)
 (sdl2 render)
 (sdl2 surface)
 (sdl2 events)
 (sdl2 video))



(sdl-init)
(create-context)
(define style (get-style))
(style-scaleallsizes style 2)

(define s-window (make-window #:opengl? #t
                              #:resizable? #t
                              #:title "demo"
                              #:high-dpi? #t))
(define s-context (make-gl-context s-window))
(define (set-gl-attributes)
  (set-gl-attribute! 'context-major-version 2)
  (set-gl-attribute! 'context-minor-version 0)
  (set-gl-attribute! 'context-flags 0)
  (set-gl-attribute! 'context-profile-mask 2)
  (set-gl-attribute! 'double-buffer 1)
  (set-gl-attribute! 'depth-size 24)
  (set-gl-attribute! 'stencil-size 8)
  (set-gl-swap-interval! 'vsync))
(set-gl-attributes)

(define (init)
  (impl:sdl2:init-opengl
   ((@@ (sdl2 video) unwrap-window) s-window)
   ((@@ (sdl2 video) unwrap-gl-context) s-context))
  (impl:opengl3:init))
(init)

(define done? (make-parameter #f))

(define (do-poll-event)
  (define %sdl-event ((@@ (sdl2 events) make-sdl-event)))
  (define sdl-event-ptr (ffi:bytevector->pointer %sdl-event))
  (let loop ((event (bind:sdl-poll-event sdl-event-ptr)))
    ;; (when event (pk 'event event))
    ;; (when event
    ;;   (when (or (quit-event? event) (window-closed-event? event))
    ;;     (set! done? #t))
    ;;   (loop (poll-event))
    ;;   )
    (impl:sdl2:process-event sdl-event-ptr)
    (unless (= 1 event)
      (loop (bind:sdl-poll-event sdl-event-ptr)))))

(define (do-new-frame)
  (impl:opengl3:new-frame)
  (impl:sdl2:new-frame))

(define (do-render)
  (set-gl-clear-color 0 0 0 240)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (impl:opengl3:render-draw-data)
  (swap-gl-window s-window))

(define (shutdown)
  (impl:opengl3:shutdown)
  (impl:sdl2:shutdown)
  (destroy-context)
  (sdl-quit))

(let ((checkbox-checked? (make-parameter #f))
      (input-n (make-parameter 0))
      (input-2 (make-parameter 0.0))
      (select-1 (make-parameter #f)))
  (while (not (done?))
    (do-poll-event)
    (do-new-frame)
    (new-frame)
    (let ((n (io-display-size (get-io))))
      (set-next-window-size
       (car n)
       (cdr n)))
    (set-next-window-pos 0 0 0 0 0)
    (when (and (checkbox-checked?)
               (begin-main-menu-bar))
      (with-menu ("hh" #t)
        (menu-item "bf" "Ctrl+N" #t #t)
        (menu-item "2" "bb" #f #t)
        (with-menu ("ba" #t)
          (menu-item "2" "bb" #f #t)))
      (when (menu-item "hello" "Ctrl+N" #f #t)
        (pk 'clock)
        (done? #t))
      (end-main-menu-bar))

    (with-window ("aba" #f ImGuiWindowFlags_AlwaysAutoResize)
      (with-popup ("a-popup")
        (text "select1")
        (selectable "1" #f)
        (selectable "3" #f))
      (with-child-window "bb"
        (with-list-box ("select" 50 0)
          (selectable "clock" select-1)
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
       (let ((cliceed (checkbox "check heerer!" checkbox-checked?))
             (cliceed-i (input-int (format #f "value is ~a" (input-n)) input-n 1 100))
             (cliceed-2 (input-float (format #f "value is ~a" (input-2)) input-2 20 100)))
         (with-combo ("combo" "2")
           (selectable "c" #f)
           (selectable "b" #f)
           (selectable "d" #f))
         (when (button "hello:")
           (open-popup "a-popup"))
         (when (checkbox-checked?)
           (sameline 0 50)
           (text "world")))
       (small-button ">")
       (sameline)
       (selectable "h" #f)
       (when (checkbox-checked?)
         (item-tooltip
          (text "tooltip")))
       (progress-bar 0.5 (cons 0 0) "hov")

       (when (textlink "https//a不过")
         (pk 'clock)
         (done? #t))
       (textlink-open-url "author" "https://github.com/z572"))

      (newline)
      (separator))

    (render)

    (let* ((w (io-display-size (get-io))))
      (gl-viewport 0 0 (car w) (cdr w)))
    (do-render)))

(shutdown)

;; Local Variables:
;; eval: (put 'with-window 'scheme-indent-function 1)
;; eval: (put 'with-child-window 'scheme-indent-function 1)
;; eval: (put 'with-menu 'scheme-indent-function 1)
;; eval: (put 'with-popup 'scheme-indent-function 1)
;; eval: (put 'with-list-box 'scheme-indent-function 1)
;; eval: (put 'with-combo 'scheme-indent-function 1)
;; End:
