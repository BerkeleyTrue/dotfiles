(module plugins.init
  {:require {r r}
   :require-macros [macros]})

(defn main []
  (->>
    [:colorizer
     :beacon
     :terraform]
    (r.map #(.. :plugins "." $1))
    (r.forEach #(run-main $1))))

(comment (main))
