(define-module (glfw)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:export (glfwinit
            glfwwindowhint
            GLFW_CONTEXT_VERSION_MAJOR
            GLFW_CONTEXT_VERSION_MINOR
            make-window
            make-context-current
            ;;glfwmakecontextcurrent
            set-swap-interval!
            window-should-close?
            ;; glfwwindowshouldclose
            poll-events
            swap-buffers
            destroy-window
            terminate
            window?
            get-cursor-pos
            unwrap-window))

(define-wrapped-pointer-type <window>
  window?
  wrap-window unwrap-window
  (lambda (window port)
    (format port "#<window size: ~s>"
            (window-size window))))

(load-extension "libguile_glfw.so" "init_glfw")

(define* (make-window #:key
                      (title "Guile GLFW Window")
                      (size '(640 480)))
  (let ((w (glfwcreatewindow (first size) (second size) title)))

    (wrap-window w)))

(define-inlinable (maybe-unwrap-window w)
  (if (window? w)
      (unwrap-window w)
      w))

(define (destroy-window w)
  (glfwdestroywindow (maybe-unwrap-window w)))

(define (window-should-close? w)
  (glfwwindowshouldclose (maybe-unwrap-window w)))
(define (window-size w)
  (%get-window-size (unwrap-window w)))
(define (make-context-current w)
  (glfwmakecontextcurrent (maybe-unwrap-window w)))
(define (swap-buffers w)
  (glfwswapbuffers (maybe-unwrap-window w)))

(define (get-cursor-pos w)
  (%get-cursor-pos (maybe-unwrap-window w)))

(define (set-swap-interval! interval)
  (swap-interval (match interval
                   ('immediate 0)
                   ('vsync 1)
                   ('late-swap-tear -1)
                   (else interval))))
