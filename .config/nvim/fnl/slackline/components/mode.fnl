(module slackline.components.mode
  {require
   {: utils
    : r
    mode slackline.mode-map
    hl slackline.highlight}})


(def- mode-map mode.m)

(defn- get-conf []
  (let [curmode (utils.fn.mode)
        ; guard against <C-V>
        guard (if (= curmode (utils.fn.eval "\"\\<C-V>\"")) :\<C-V> curmode)]
      (or (. mode-map guard) {})))

(defn- render-mode []
  (let [conf (get-conf)
        mode-name (or (. conf :name) "NA")]
    (..
      (hl.hl-comp mode-name)
      "%-9( "
      mode-name
      " %)")))

(defn- get-colors []
  (->>
    mode-map
    (r.vals)
    (r.map (fn [{: bg : name}] {: bg : name}))))

(defn- get-current-color []
  (. (get-conf) :name))

(defn main [child? args]
  (let [{: active} (or args {})
        child (if (r.function? child?) child? r.noop)]
    {:name :mode
     :render (if active render-mode #"")
     :next (child {: active : get-colors : get-current-color})
     :init
     (fn []
       (->>
         mode-map
         (r.vals)
         (r.for-each
           (fn [{: name : fg : bg}]
             (hl.add-group name fg bg)))))}))
