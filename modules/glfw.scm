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
            set-window-title!
            get-window-title
            window-title
            window-size
            get-window-size
            set-window-size!
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

(define (get-window-size w)
  (%get-window-size (maybe-unwrap-window w)))
(define (set-window-size! w o)
  (%set-window-size (maybe-unwrap-window w) (first o) (second o)))
(define window-size
  (make-procedure-with-setter get-window-size set-window-size!))

(define (make-context-current w)
  (glfwmakecontextcurrent (maybe-unwrap-window w)))
(define (swap-buffers w)
  (glfwswapbuffers (maybe-unwrap-window w)))

(define (get-cursor-pos w)
  (%get-cursor-pos (maybe-unwrap-window w)))

(define (set-window-title! w title)
  (%set-window-title (maybe-unwrap-window w) title))

(define (get-window-title w)
  (%get-window-title (maybe-unwrap-window w)))

(define window-title
  (make-procedure-with-setter get-window-title set-window-title!))

(define (set-swap-interval! interval)
  (swap-interval (match interval
                   ('immediate 0)
                   ('vsync 1)
                   ('late-swap-tear -1)
                   (else interval))))
