(define-module (imgui inputs)
  #:use-module (imgui)
  #:use-module (srfi srfi-34)
  #:export (key-down?
            key-pressed?
            key-released?
            key-name

            mouse-down?
            mouse-released?
            mouse-clicked?
            mouse-double-clicked?))

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

(define (mouse-down? m)
  (assert-context)
  (%mouse-down? m))

(define (mouse-released? m)
  (assert-context)
  (%mouse-released? m))

(define (mouse-clicked? m)
  (assert-context)
  (%mouse-clicked? m))

(define (mouse-double-clicked? m)
  (assert-context)
  (%mouse-double-clicked? m))
