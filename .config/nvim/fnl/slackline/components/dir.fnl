(module slackline.components.dir
  {:require {: r
             : utils}})


(defn- render-dir []
  (let [curbuf (. utils.g :actual_curbuf)
        wd (utils.fn.expand "%:.")
        pd (utils.fn.expand "%:.:h:t")
        fln (utils.fn.expand "%:t")]
    (if
      (< (length wd) 14) (.. " " wd)
      (.. "../" pd "/" fln))))

(defn main [child? args]
  (let [{: active} (or args {})
        child (if (r.function? child?) child? r.noop)]
    {:name :dir
     :render render-dir
     :next (child args)}))
