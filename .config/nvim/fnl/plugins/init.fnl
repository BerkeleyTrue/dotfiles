(module plugins.init
  {:require {a aniseed.core}
   :require-macros [macros]})

(defn main []
  (->>
    [:plugins.colorizer
     :plugins.beacon
     :plugins.terraform]
    (a.map #(run-main $1))))

(comment (main))
