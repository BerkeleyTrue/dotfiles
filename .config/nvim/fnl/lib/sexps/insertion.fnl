(module lib.sexps.insertion
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    forms lib.sexps.forms
    tsnode lib.treesitter.tsnode}
   require {}
   require-macros [macros]})

(defn head []
  (let [form (forms.find)
        {: row : col} (tsnode.start form)
        start-col (+ col 1)]
    (n buf-set-text 0 row start-col row start-col [" "])
    (n win-set-cursor 0 [(+ row 1) start-col])
    (n feedkeys :i :n true)))

(comment
  (nmap :<leader>ih head))

(defn tail []
  (let [form (forms.find)
        {: row : col} (forms.end form)
        end-col (- col 1)]
    (n buf-set-text 0 row end-col row end-col [" "])
    (n win-set-cursor 0 [(+ row 1) (+ col 1)])
    (n feedkeys :i :n true)))

(comment
  (nmap :<leader>it tail))
