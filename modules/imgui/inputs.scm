(define-module (imgui inputs)
  #:use-module (imgui)
  #:use-module (srfi srfi-34)
  #:export (key-down?
            key-pressed?
            key-released?
            key-name))

(load-extension "libguile_dear_imgui.so" "init_imgui_inputs")


(define (key-down? key)
  (assert-context)
  (%key-down? key))

(define (key-pressed? key)
  (assert-context)
  (%key-pressed? key))

(define (key-released? key)
  (assert-context)
  (%key-released? key))

(define (key-name key)
  (assert-context)
  (%key-name key))
