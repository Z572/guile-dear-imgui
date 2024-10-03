(use-modules (imgui draw))
(example
 "draw"
 (with-window ("draw")
   (let ((d (window-draw-list)))
     (add-line d '(0 0) '(300 300) '(255 255 255 255))
     (add-rect d '(0 0) '(301 301) '(255 255 255 255) 2))))
