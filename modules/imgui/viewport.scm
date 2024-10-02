(define-module (imgui viewport)
  #:use-module (system foreign)
  #:export (viewport?
            wrap-viewport
            unwrap-viewport
            main-viewport))

(define-wrapped-pointer-type <viewport>
  viewport?
  wrap-viewport unwrap-viewport
  (lambda (b p)
    (format p "#<viewport ~x>"
            (pointer-address (unwrap-viewport b)))))

(load-extension "libguile_dear_imgui.so" "init_imgui_viewport")

(define (main-viewport)
  (wrap-viewport (%get-main-viewport)))
