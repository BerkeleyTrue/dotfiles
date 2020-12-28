(module slackline.components.dir
  {:require {: r
             : utils}
   :require-macros [macros]})

(defn render-in-context []
  (let [bufname (utils.fn.bufname "%")
        unlisted? (= (utils.fn.buflisted bufname) 0)
        wd (utils.fn.expand "%:.")
        pd (utils.fn.expand "%:.:h:t")
        fln (utils.fn.expand "%:t")]
    (if
      unlisted? ""
      (< (length wd) 14) (.. " " wd)
      (.. "../" pd "/" fln))))

(defn- render-dir []
  (..
    "%{"
    (utils.viml->luaexp *module-name* (sym->name render-in-context))
    "}"))

(defn main [child? args]
  (let [{: active} (or args {})
        child (if (r.function? child?) child? r.noop)]
    {:name :dir
     :render render-dir
     :next (child {: active})}))
