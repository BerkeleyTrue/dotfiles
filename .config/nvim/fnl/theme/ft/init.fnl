(module theme.ft
  {:require {r r}
   :require-macros [macros]})

(defn main [utils]
  (->>
    [:js
     :markdown
     :spell
     :yaml]
    (r.map #(.. *module-name* "." $1))
    (r.forEach #(run-main $1 utils))))

(comment (main))
