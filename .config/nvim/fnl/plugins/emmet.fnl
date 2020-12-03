(module plugins.emmet
  {:require {r r
             nvim aniseed.nvim}})

(defn main []
  (->>
    {:user_emmet_install_global 0
     :user_emmet_settings {:html {:quote_char "'"}
                           :jsx {:quote_char "'"}
                           :javascript.jsx {:extends "jsx"}}}
    (r.to-pairs)
    (r.forEach
      (fn [[key val]] (tset nvim.g key val)))))
