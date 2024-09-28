(example "tree"
         (when (collapsing-header "header")
           (text "abc"))
         (tree-node "a")
         (when (tree-push "label")
           (tree-pop)))
