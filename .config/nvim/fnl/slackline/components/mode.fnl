(module slackline.components.mode
  {:require {: utils
             : r
             mode slackline.mode-map
             hl slackline.highlight}})


(def- mode-map mode.m)

(defn render-mode []
  (let [curmode (utils.fn.mode)
        mode-name (. (. mode-map curmode) :name)]
    (..
      (hl.hl-comp mode-name)
      "%8("
      mode-name
      "%)"
      :%#StatusLine#)))


(defn main [child? args]
  (let [{: active} (or args {})
        child (if (r.function? child?) child? r.noop)]

    {:name :mode
     :render (if active render-mode #"")
     :next (child args)
     :init
     (fn []
       (->>
         mode-map
         (r.vals)
         (r.for-each
           (fn [{: name : fg : bg}]
             (hl.add-group name fg bg)))))}))
