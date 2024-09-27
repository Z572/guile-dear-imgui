(define-module (imgui)
  #:use-module (system foreign)
  #:use-module ((rnrs base) #:select (assert))
  #:export (create-context
            open-popup
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
            set-io-display-size
            io-fonts
            io-fonts-get-texdata-as-rgba32
            add-font-from-file!
            begin-list-box
            end-list-box
            begin-group
            end-group
            selectable
            color-edit3
            color-edit4
            color-picker3
            color-picker4
            sameline
            indent
            unindent
            text
            text-colored
            newline
            get-font-size
            bullet
            get-version
            checkbox
            input-int
            input-float
            begin-combo
            end-combo
            begin-main-menu-bar
            end-main-menu-bar
            ;; begin-tab-bar
            ;; end-tab-bar
            ;; begin-tab-item
            ;; end-tab-item
            input-text
            drag-int
            slider-int
            menu-item
            button
            small-button
            progress-bar
            begin-table
            end-table
            table-next-row
            table-next-column
            table-column-index
            table-setup-column
            table-headers-row
            table-header
            textlink
            textlink-open-url
            separator
            render
            default-focus
            keyboard-focus-here!
            show-font-selector
            show-style-selector
            destroy-context)
  #:export-syntax (group
                   with-window
                   with-child-window
                   with-combo
                   with-menu
                   with-popup
                   with-list-box
                   tab-bar
                   tab-item
                   table
                   item-tooltip))

(define-wrapped-pointer-type <context>
  context?
  wrap-context unwrap-context
  (lambda (b p)
    (format p "#<context ~x>"
            (pointer-address (unwrap-context b)))))

(load-extension "libguile_dear_imgui.so" "init_imgui")

(define (create-context)
  (wrap-context (%create-context)))

(define* (destroy-context #:optional (ctx #f))
  (if ctx
      (%destroy-context (unwrap-context ctx))
      (%destroy-context)))

(define-syntax-rule (with-window (name args ...) body ...)
  (begin (when (begin-window name args ...)
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

(define-syntax tab-bar
  (syntax-rules ()
    ((_ (name args ...) body ...)
     (when (begin-tab-bar name args ...)
       (syntax-parameterize ((tab-item (with-ellipsis :::
                                         (syntax-rules ()
                                           ((tab-item (n args2 :::) body2 :::)
                                            (when (begin-tab-item n args2 :::)
                                              body2 :::
                                              (end-tab-item)))))))
         body ...)
       (end-tab-bar)))))

(define-syntax-parameter tab-item
  (lambda (stx)
    (syntax-violation 'tab-item "tab-item used outside of a tab-bar" stx)))

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

(define-syntax-rule (table (id column args ...) body ...)
  (when (begin-table id column args ...)

    body ...
    (end-table)))

(define* (input-text label buf #:optional (flags 0)
                     #:key
                     (hint #f)
                     (callback #f))
  (if callback (error "callback no impl!"))
  (if hint
      (input-text-with-hint label hint buf flags)
      (%input-text label buf flags)))

(define* (add-font-from-file! f filename size
                              #:key
                              (font-config %null-pointer)
                              (ranges #f))
  (assert (file-exists? filename))
  (assert (or (string-suffix-ci? ".ttf" filename)
              (string-suffix-ci? ".otf" filename)))
  (ImFontAtlasAddFontFromFileTTF f filename size font-config ranges))

(define table-column-index
  (make-procedure-with-setter
   get-table-column-index
   set-table-column-index!))
