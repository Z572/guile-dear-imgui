(use-modules (imgui))
(create-context)
(let ((io (get-io)))
  (set-io-display-size io 1920 1080)
  (io-fonts-get-texdata-as-rgba32 (io-fonts io)
                                  'ignored
                                  'ignored
                                  'ignored))

(let loop ((n 20))
  (format #t "newframe: ~a~%" n)
  (new-frame)
  (text "Hello, world!")
  (render)
  (when (< 0 n )
    (loop (- n 1))))
(destroy-context)
(exit 0)
