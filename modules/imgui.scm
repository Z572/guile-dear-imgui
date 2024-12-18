(define-module (imgui)
  #:use-module (system foreign)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module ((rnrs base) #:select (assert))
  #:export (create-context
            current-context
            open-popup
            get-io
            get-style
            style-scale-all-sizes
            new-frame
            io-display-size
            begin-window
            show-metrics-window
            show-demo-window
            show-style-editor
            show-user-guide
            image
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
            dummy
            calc-text-size
            calc-item-width
            indent
            unindent
            text
            label-text
            text-colored
            text-wrapped
            text-disabled
            bullet-text
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
            invisible-button
            progress-bar
            begin-tooltip
            begin-item-tooltip
            end-tooltip
            begin-table
            end-table
            table-next-row
            table-next-column
            table-column-index
            table-setup-column
            table-setup-scroll-freeze
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
            destroy-context
            tree-node
            tree-push
            tree-pop
            collapsing-header
            align-text-to-frame-padding

            scroll-x
            scroll-y
            set-scroll-x!
            set-scroll-y!
            scroll-max-x
            scroll-max-y

            cursor-start-position
            cursor-screen-position
            cursor-position
            set-cursor-screen-position!
            set-cursor-position!
            content-regin-avail


            get-text-line-height
            get-text-line-height-with-spacing
            get-time
            get-frame-height
            get-frame-height-with-spacing
            list-clipper
            list-clipper-begin
            list-clipper-step
            list-clipper-display-start
            list-clipper-display-end
            draw-data
            assert-context)
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
                   item-tooltip
                   with-style
                   with-style-colors))

(define-condition-type &imgui-no-context &message
  &imgui-no-context-condition?)

(define-inlinable (assert-context)
  (unless (current-context)
    (raise (condition
            (&imgui-no-context
             (message "Not in a context!"))))))

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

(define (current-context)
  (and=> (%current-context) wrap-context))

(define-syntax-rule (im-catch begin end body ...)
  (let ((yes #f)
        (ended? #f))
    (assert-context)
    (catch #t
      (lambda ()
        (when begin
          (set! yes #t)
          (let ()
            *unspecified*
            body
            ...
            end
            (set! ended? #t))))
      (lambda (key . args)
        (when (and yes (not ended?))
          end)
        (apply throw key args)))))

(define* (begin-window name #:key
                       (open? #f)
                       (flags 0))
  (when open?
    (assert (procedure? open?)))
  (%begin-window
   name
   open?
   flags))

(define-syntax-rule (with-window (name args ...) body ...)
  (let ((ended? #f))
    (assert-context)
    (catch #t
      (lambda ()
        (when (begin-window name args ...)
          (let ()
            *unspecified*
            body ...))
        (unless ended?
          (end-window)
          (set! ended? #t)))
      (lambda (key . _args)
        (unless ended?
          (end-window)
          (set! ended? #t))
        (apply throw key _args)))))

(define-syntax-rule (with-child-window (name args ...) body ...)
  (let ((ended? #f))
    (assert-context)
    (catch #t
      (lambda ()
        (when (begin-child name args ...)
          (let ()
            *unspecified*
            body ...))
        (unless ended?
          (end-child)
          (set! ended? #t)))
      (lambda (key . _args)
        (unless ended?
          (end-child)
          (set! ended? #t))
        (apply throw key _args)))))

(define-syntax-rule (with-list-box (name args ...) body ...)
  (im-catch (begin-list-box name args ...)
            (end-list-box)
            body ...))

(define-syntax-rule (group body ...)
  (im-catch (begin-group) (end-group) body ...))

(define-syntax-rule (tooltip body ...)
  (im-catch (begin-tooltip) (end-tooltip) body ...))

(define-syntax tab-bar
  (syntax-rules ()
    ((_ (name args ...) body ...)
     (im-catch (begin-tab-bar name args ...)
               (end-tab-bar)
               (syntax-parameterize ((tab-item
                                         (with-ellipsis :::
                                           (syntax-rules ()
                                             ((tab-item (n args2 :::) body2 :::)
                                              (im-catch
                                               (begin-tab-item n args2 :::)
                                               (end-tab-item)
                                               body2 :::))))))
                 body ...)))))

(define-syntax-parameter tab-item
  (lambda (stx)
    (syntax-violation 'tab-item "tab-item used outside of a tab-bar" stx)))

(define-syntax-rule (with-combo (label preview_value ) body ...)
  (im-catch (begin-combo label preview_value)
            (end-combo)
            body ...))
(define-syntax-rule (with-menu (label act) body ...)
  (im-catch (begin-menu label act)
            (end-menu)
            body ...))

(define-syntax-rule (with-popup (label) body ...)
  (im-catch (begin-popup label)
            (end-popup)
            body ...))
(define-syntax-rule (item-tooltip body ...)
  (im-catch (begin-item-tooltip)
            (end-tooltip)
            body ...))

(define-syntax-rule (table (id column args ...) body ...)
  (im-catch (begin-table id column args ...)
            (end-table)
            body ...))

(define* (input-text label buf #:optional (flags 0)
                     #:key
                     (hint #f)
                     (callback #f)
                     (multiline #f))

  (when hint
    (assert (string? hint)))
  (when callback
    (assert (procedure? callback)))
  (when multiline
    (assert (= 2 (length multiline))))
  (%inputex label buf hint flags multiline callback))

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


(define-syntax with-style-colors
  (lambda (x)
    (syntax-case x ()
      ((_ ((color value))
          body ...)
       #`(if (PushStyleColor (symbol->string 'color) value)
             (begin body ... (PopStyleColor))
             (error "unknown style color! ~a" 'color)))
      ((_ ((color value) o ...)
          body ...)
       #`(with-style-colors ((color value))
           (with-style-colors (o ...)
             body ...))))))

(define-syntax with-style
  (lambda (x)
    (syntax-case x ()
      ((_ ((var value))
          body ...)
       #`(if (PushStyleVar (symbol->string 'var) value)
             (begin body ... (PopStyleVar))
             (error "unknown style var! ~a" 'color)))
      ((f ((var value) o ...)
          body ...)
       #`(f ((var value))
            (f (o ...)
               body ...))))))

(define set-cursor-position!
  (case-lambda ((vec) (%set-cursor-position! (first vec) (second vec)))
               ((x y) (%set-cursor-position! x y))))
(define cursor-position
  (make-procedure-with-setter
   %cursor-position set-cursor-position!))

(define set-cursor-screen-position!
  (case-lambda ((vec) (%set-cursor-screen-position! (first vec) (second vec)))
               ((x y) (%set-cursor-screen-position! x y))))
(define cursor-screen-position
  (make-procedure-with-setter
   %cursor-screen-position set-cursor-screen-position!))

(define image
  (case-lambda ((id vec) (%image id (first vec) (second vec)))
               ((id x y) (%image id x y))))

(define (draw-data)
  ((@@ (imgui draw) wrap-draw-data)
   (%draw-data)))

;; Local Variables:
;; eval: (put 'with-window 'scheme-indent-function 1)
;; eval: (put 'with-child-window 'scheme-indent-function 1)
;; eval: (put 'with-menu 'scheme-indent-function 1)
;; eval: (put 'with-popup 'scheme-indent-function 1)
;; eval: (put 'tab-bar 'scheme-indent-function 1)
;; eval: (put 'tab-item 'scheme-indent-function 1)
;; eval: (put 'with-list-box 'scheme-indent-function 1)
;; eval: (put 'with-combo 'scheme-indent-function 1)
;; eval: (put 'with-style 'scheme-indent-function 1)
;; eval: (put 'with-style-colors 'scheme-indent-function 1)
;; eval: (put 'with-ellipsis 'scheme-indent-function 1)
;; End:
