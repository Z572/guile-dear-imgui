(define-module (imgui item)
  #:use-module (srfi srfi-1)
  #:export (item-hovered?
            item-active?
            item-focused?
            item-clicked?
            item-visible?
            item-edited?
            item-activated?
            item-deactivated?
            item-deactivated-after-edit?
            item-toggle-open?
            any-item-hovered?
            any-item-active?
            any-item-focused?
            item-id
            item-rect-min
            item-rect-max
            item-rect-size))

(load-extension "libguile_dear_imgui.so" "init_imgui_item")
