(module plugins.sexp
  {:require {utils utils}})

(defn main []
  (utils.set-nvim-g! {:sexp_filetypes "clojure,fennel"}))
