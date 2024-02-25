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

(defn- delete-space [row start end]
  (let [start (or start (- end 1))
        end (or end (+ start 1))]
    (n buf-set-text 0 row start row end [])))

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

(comment (nmap ")<" barf-forward))

; TODO: handle multi line forms
(defn barf-backward []
  (when-let [form (forms.find)
             {: row : col} (tsnode.start form)
             first (tsnode.first form)
             firstr (tsnode.range first)
             text (.. (tsnode.text first) " ")]
    (tsnode.delete first)
    (delete-space firstr.start.row firstr.start.col)
    (insert-text row col [text])))

(comment (nmap "(>" barf-backward))

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
  (nmap ")>" slurp-forward {:desc "Slurp element forward"}) ; foo
  (let [:foo :bar]
    (print :foo)))

(defn slurp-backward []
  (when-let [form (forms.find)
             {: row : col} (tsnode.start form)]

    (when-let [sibling (forms.prev-elem form)
               srange (tsnode.range sibling)
               text (.. (tsnode.text sibling) " ")]
      (insert-text row (+ col 1) [text])
      (tsnode.delete sibling)
      (delete-space srange.start.row srange.start.col))))

(comment
  (nmap "<(" slurp-backward))

(defn wrap-form []
  (when-let [form (forms.find)
             {: start : end} (tsnode.range form)]
    (insert-text start.row start.col ["( "])
    (insert-text end.row end.col [")"])
    (n win-set-cursor 0 [(+ start.row 1) (+ start.col 1)])
    (n feedkeys :i :n true)))

(comment
  (nmap :<localleader>w wrap-form))

(defn unwrap-form []
  (when-let [form (forms.find)]
    (when-let [parent (forms.parent form)
               text (->>
                      (tsnode.text form)
                      (r.split "\n"))
               {: start : end} (tsnode.range parent)]
      (n buf-set-text 0 start.row start.col end.row end.col text)
      (n win-set-cursor 0 [(+ start.row 1) start.col]))))

(comment
  (nmap :<localleader>u unwrap-form))

(defn unwrap-element []
  (let [element (ts.get-node-under-cursor)
        parent (forms.parent element)
        offset (forms.->offset parent)
        text (tsnode.text element)
        {: start : end} (tsnode.range parent)]
    (n buf-set-text 0 start.row start.col end.row end.col [text])
    (n win-set-cursor 0 [(+ start.row 1) start.col])))

(comment
  (nmap :<localleader>U unwrap-element))
