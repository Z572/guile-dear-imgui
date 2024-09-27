(use-modules (imgui))
(define-syntax-rule (example name
                             body ...)
  (begin (create-context)
         (let ((io (get-io)))
           (set-io-display-size io 1920 1080)
           (io-fonts-get-texdata-as-rgba32 (io-fonts io)
                                           'ignored
                                           'ignored
                                           'ignored))

         (new-frame)
         body ...
         (render)
         (destroy-context)))
