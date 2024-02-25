(module lib.sexps.operation
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

(defn around-form []
  (when-let [form (forms.find)]
    (ts.update-selection form)))

(comment
  (omap :af around-form {:desc "Select around a form"}))

(defn around-root-form []
  (when-let [form (forms.find-root)]
    (ts.update-selection form)))

(comment
  (omap :ar around-root-form {:desc "Select around the root form"}))

(defn- in-this-form [form]
  (let [{: start : end} (tsnode.range form)
        offset (forms.->offset form)
        mode (vf mode)
        selecting? (r.includes? [:v :vs :V :Vs :s :S] mode)]
    (n win-set-cursor 0 [(+ start.row 1) (+ start.col offset)])
    (when-not selecting?
      (n command "normal! v"))
    (n win-set-cursor 0 [(+ end.row 1) (- end.col 2)])))

(comment
  (in-this-form (forms.find))
  (in-this-form (forms.find-root)))

(defn in-form []
  (when-let [form (forms.find)]
    (in-this-form form)))

(comment
  (omap :if in-form {:desc "Select in a form"})
  (xmap :if in-form {:desc "Select in a form"}))

(defn in-root-form []
  (when-let [form (forms.find-root)]
    (in-this-form form)))

(comment
  (omap :ir in-root-form {:desc "Select in the root form"}))
