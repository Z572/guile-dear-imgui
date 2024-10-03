(define-module (imgui draw)
  #:use-module (system foreign)
  #:export (window-draw-list
            add-line
            add-rect))

(define-wrapped-pointer-type <draw-list>
  draw-list?
  wrap-draw-list unwrap-draw-list
  (lambda (b p)
    (format p "#<draw-list ~x>"
            (pointer-address (unwrap-draw-list b)))))

(load-extension "libguile_dear_imgui.so" "init_imgui_draw")

(define (window-draw-list)
  (wrap-draw-list (%window-draw-list)))

(define* (add-line draw-list p1 p2 color #:optional (thickness 1.0))
  (%add-line (unwrap-draw-list draw-list) p1 p2 color thickness))

(define* (add-rect draw-list pmin pmax color
                   #:optional
                   (rounding 0.0)
                   (flags 0)
                   (thickness 1.0))
  (%add-rect (unwrap-draw-list draw-list) pmin pmax color rounding flags thickness))
