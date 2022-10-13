(module plugins
  {require {r r}
   require-macros [macros]})

(def- plugins
  [:accents
   :scroll-fix
   :mumber
   :move])

(defn main []
  (->>
   plugins
   (r.map #(.. *module-name* "." $1))
   (r.forEach #(run-main $1))))

(comment (main))
