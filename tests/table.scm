(include "helper.scm")
(example
 "table"
 (when (begin-table "t" 2)
   (table-next-column)
   (text "helo")
   (table-next-column)
   (text "w")
   (table-next-column)
   (text "b")
   (table-next-column)
   (text "k")
   (end-table)))
