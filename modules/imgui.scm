(define-module (imgui)
  #:use-module (system foreign)
  #:export (create-context
            get-io
            get-style
            style-scaleallsizes
            new-frame
            io-display-size
            set-next-window-size
            set-next-window-pos
            begin-window
            end-window
            begin-popup
            end-popup
            begin-child
            end-child

            begin-list-box
            end-list-box
            begin-group
            end-group
            selectable
            sameline
            indent
            unindent
            text
            newline
            get-font-size
            bullet
            get-version
            checkbox
            input-int
            input-float
            begin-combo
            end-combo
            button
            small-button
            textlink
            textlink-open-url
            separator
            render
            impl:opengl3:shutdown
            impl:sdl2:shutdown
            destroy-context
            impl:opengl3:render-draw-data
            impl:sdl2:init-opengl
            impl:opengl3:init
            impl:sdl2:process-event
            impl:sdl2:new-frame
            impl:opengl3:new-frame
            impl:glfw:init-opengl
            impl:glfw:new-frame
            impl:glfw:shutdown)
  #:export-syntax (group
                   with-window
                   with-child-window
                   with-combo
                   with-menu
                   with-popup
                   with-list-box))

(define-wrapped-pointer-type <context>
  context?
  wrap-context unwrap-context
  (lambda (b p)
    (format p "#<context ~x>"
            (pointer-address (unwrap-context b)))))

(load-extension "build/libguile_dear_imgui.so" "init_imgui")

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
(define-syntax-rule (with-combo (label preview_value ) body ...)
  (when (begin-combo label preview_value)
    body ...
    (end-combo)))
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
