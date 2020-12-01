(module plugins.init
  {:require {a aniseed.core}
   :require-macros [macros]})

(defn main []
  (->> [:plugins.colorizer
        :plugins.beacon]
    (a.map #(run-main $1))))

(comment (main))
