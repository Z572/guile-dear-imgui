(example "tab-bar"
         (tab-bar ("bar1")
           (false-if-exception
            (tab-item ("bar2")
              f))
           (tab-item ("bar0"))))
