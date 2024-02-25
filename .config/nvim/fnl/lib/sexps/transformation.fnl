(module lib.sexps.transformation
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    forms lib.sexps.forms
    ts lib.treesitter
    tsnode lib.treesitter.tsnode}
   require {}
   require-macros [macros]})

(defn- insert-text [row col text]
  (n buf-set-text 0 row col row col text))

(defn barf-forward []
  "Barf the last element of a form outward."
  (when-let [form (forms.find)
             formr (tsnode.range form)
             last (tsnode.last form)
             lastr (tsnode.range last)
             dedent? (< formr.start.row
                        lastr.start.row
                        lastr.end.row)
             text (tsnode.text last)]
    (if dedent?
      (vim.fn.execute (.. (+ lastr.start.row 1) "<"))
      (do
        (insert-text
          formr.end.row
          formr.end.col
          [(.. " " text)])
        (tsnode.delete last)
        ; delete preceding whitespace
        (n buf-set-text 0
           lastr.start.row (- lastr.start.col 1)
           lastr.start.row lastr.start.col
           [])))))

(comment (nmap "<)" barf-forward))

(defn barf-backward []
  (when-let [form (forms.find)
             first (tsnode.first form)
             text (.. (tsnode.text first) " ")
             [row-start col-start row-end col-end] [(first:range)]
             _ (n buf-set-text 0 row-start col-start row-end (+ col-end 1) [""])
             nform (forms.find)
             [nrow-start ncol-start] [(nform:start)]]
    (n buf-set-text 0 nrow-start ncol-start nrow-start ncol-start [text])))

(comment (nmap ">(" barf-backward))

; TODO: use indenting instead and let parinfer manage parens
(defn slurp-forward []
  "Slurp the next sibling into the current form"
  (when-let [form (forms.find)
             {: row : col} (tsnode.end form)
             col (- col 1)] ; col is end exclusive

    (when-let [sibling (forms.next-elem form)
               {: start
                : end} (tsnode.range sibling)
               indent? (< row start.row end.row)
               text (->>
                     (tsnode.text sibling)
                     (r.split "\n")
                     (r.map (fn [line] (.. (if indent? "  " " ") line))))]

      (tsnode.delete sibling)

      (if indent?
        (insert-text start.row start.col text)
        (insert-text row col text)))))

(comment
  (nmap ">)" slurp-forward {:desc "Slurp element forward"}) ; foo
  (let [:foo :bar]
    (print :foo)))

(defn slurp-backward []
  (let [form (forms.find)
        {: row : col} (tsnode.start form)
        offset (forms.->offset form)
        col (+ col offset)
        sibling (form:prev_sibling)]
    (when sibling
      (let [text (->
                   (tsnode.text sibling)
                   (r.split "\n")
                   (r.map (fn [line] (.. line " "))))
            {: start : end} (tsnode.range sibling)]
        (n buf-set-text 0 row col row col text)
        (n buf-set-text 0 start.row start.col end.row (+ end.col 1) [""])))))

(defn wrap-form []
  (let [form (forms.find)
        offset (forms.->offset form)
        {: start : end} (tsnode.range form)]
    (n buf-set-text 0 start.row start.col start.row start.col ["( "])
    (n buf-set-text 0 end.row end.col end.row end.col [")"])
    (n win-set-cursor 0 [(+ start.row 1) (+ start.col offset)])
    (n feedkeys :i :n true)))

(defn unwrap-form []
  (let [form (forms.find)
        parent (form:parent)
        text (tsnode.text form)
        {: start : end} (tsnode.range parent)]
    (n buf-set-text 0 start.row start.col end.row end.col [text])
    (n win-set-cursor 0 [(+ start.row 1) start.col])))

(defn unwrap-element []
  (let [element (ts.get-node-under-cursor)
        parent (element:parent)
        text (tsnode.text element)
        {: start : end} (tsnode.range parent)]
    (n buf-set-text 0 start.row start.col end.row end.col [text])
    (n win-set-cursor 0 [(+ start.row 1) start.col])))
