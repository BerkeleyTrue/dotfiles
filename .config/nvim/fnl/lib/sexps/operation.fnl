(module lib.sexps.operation
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    forms lib.sexps.forms
    ts lib.treesitter
    tsnode lib.treesitter.node}
   require {}
   require-macros [macros]})

(defn- in-this-form [form]
  (let [{: start : end} (tsnode.range form)
        offset (forms.->offset form)]
    (n win-set-cursor 0 [(+ start.row 1) (+ start.col offset)])
    (n command "normal! v")
    (n win-set-cursor 0 [(+ end.row 1) (- end.col 2)])))

(defn around-form []
  (let [form (forms.find)]
    (ts.update-selection form)))

(defn around-root-form []
  (let [form (forms.find-root)]
    (ts.update-selection form)))

(defn in-form []
  (inner-select (forms.find)))

(defn in-root-form []
  (inner-select (forms.find-root)))
