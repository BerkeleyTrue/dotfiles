(module lib.sexps.insertion
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    forms lib.sexps.forms}
   require {}
   require-macros [macros]})

(defn head []
  (let [form (forms.find)
        offset (forms.->offset form)
        {: row : col} (forms.start form)
        start-col (+ col offset)]
    (n buf-set-text 0 row start-col row start.col [" "])
    (n win-set-cursor 0 [(+ row 1) start-col])
    (n feedkeys :i :n true)))

(defn tail []
  (let [form (forms.find)
        {: row : col} (forms.end form)
        end-col (- col 1)]
    (n buf-set-text 0 row end-col row end-col [" "])
    (n win-set-cursor 0 [(+ end-row 1) (+ col 1)])
    (n feedkeys :i :n true)))
