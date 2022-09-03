(module plugins.sexp
  {require {utils utils}})

(defn main []
  (utils.set-nvim-g!
    {:sexp_filetypes "clojure,scheme,lisp,timl,fennel,janet"
     :sexp_enable_insert_mode_mappings 0}))
