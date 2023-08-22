(module theme.ft
  {require
   {: r}
   require-macros [macros]})

(defn main []
  (->>
    [:js
     :lsp
     :markdown
     :yaml]
    (r.map #(.. *module-name* "." $1))
    (r.forEach #(run-main $1 utils))))

(comment (main))
