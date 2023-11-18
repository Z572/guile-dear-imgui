(load-extension "build/libguile_dear_imgui.so" "init_imgui")
;; (imgui-context-set-current! (imgui-context-create))
;; (imgui-new-frame)
(pk 's (imgui:begin ;; "abc"
        ;; (list '(1) 2 3 4 )
        "aba"
        )
    )
;; (pk 'a)
;; (imgui-end)
