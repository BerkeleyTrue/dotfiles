(module lib.sexps.transformation
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    forms lib.sexps.forms
    ts lib.treesitter}
   require {}
   require-macros [macros]})

(defn- insert-text [row col text]
  (n buf-set-text 0 row col row col text))

(defn- delete-node [node]
  (let [{: start : end} (ts.range node)]
    (n buf-set-text 0 start.row start.col end.row end.col [])))

(defn barf-forward []
  (let [form (forms.find)
        last (ts.last form)
        [row col] [(form:end_)]
        {: start : end} (ts.range last)
        text (.. " " (ts.get-node-text last))]
    (n buf-set-text 0 row col row col [text])
    (n buf-set-text 0 start.row (- end.col 1) end.row end.col [""])))

(defn barf-backward []
  (let [form (forms.find)
        first (ts.first form)
        text (.. (ts.get-node-text first) " ")
        [row-start col-start row-end col-end] [(first:range)]
        _ (n buf-set-text 0 row-start col-start row-end (+ col-end 1) [""])
        nform (forms.find)
        [nrow-start ncol-start] [(nform:start)]]
    (n buf-set-text 0 nrow-start ncol-start nrow-start ncol-start [text])))

(defn slurp-forward []
  (when-let [form (forms.find)]
    (let [{: row : col} (ts.end form)
          col (- col 1)] ; col is end exclusive
      (when-let [sibling (form:next_sibling)]
        (let [{: start
               : end} (ts.range sibling)
              indent? (< row start.row end.row)
              text (->>
                     (ts.get-node-text sibling)
                     (r.split "\n")
                     (r.map (fn [line] (.. (if indent? "  " " ") line))))]

          (delete-node sibling)

          (if indent?
            (insert-text start.row start.col text)
            (insert-text row col text)))))))

(comment
  (nmap ">)" slurp-forward {:desc "Slurp element forward"})
  (let [:foo :bar]
    (print :foo)))

(defn slurp-backward []
  (let [form (forms.find)
        {: row : col} (ts.start form)
        offset (forms.->offset form)
        col (+ col offset)
        sibling (form:prev_sibling)]
    (when sibling
      (let [text (->
                   (ts.get-node-text sibling)
                   (r.split "\n")
                   (r.map (fn [line] (.. line " "))))
            {: start : end} (ts.range sibling)]
        (n buf-set-text 0 row col row col text)
        (n buf-set-text 0 start.row start.col end.row (+ end.col 1) [""])))))

(defn wrap-form []
  (let [form (forms.find)
        offset (forms.->offset form)
        {: start : end} (ts.range form)]
    (n buf-set-text 0 start.row start.col start.row start.col ["( "])
    (n buf-set-text 0 end.row end.col end.row end.col [")"])
    (n win-set-cursor 0 [(+ start.row 1) (+ start.col offset)])
    (n feedkeys :i :n true)))

(defn unwrap-form []
  (let [form (forms.find)
        parent (form:parent)
        text (ts.get-node-text form)
        {: start : end} (ts.range parent)]
    (n buf-set-text 0 start.row start.col end.row end.col [text])
    (n win-set-cursor 0 [(+ start.row 1) start.col])))

(defn unwrap-element []
  (let [element (ts.get-node-under-cursor)
        parent (element:parent)
        text (ts.get-node-text element)
        {: start : end} (ts.range parent)]
    (n buf-set-text 0 start.row start.col end.row end.col [text])
    (n win-set-cursor 0 [(+ start.row 1) start.col])))
