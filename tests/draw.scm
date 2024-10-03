(use-modules (imgui draw))
(example
 "draw"
 (with-window ("draw")
   (let ((d (window-draw-list))
         (b (background-draw-list))
         (f (foreground-draw-list)))
     (add-line d '(0 0) '(300 300) '(255 255 255 255))
     (add-rect b '(0 0) '(301 301) '(255 255 255 255) 2)
     (add-rect f '(300 300) '(600 600) '(255 255 255 255) 20))))
