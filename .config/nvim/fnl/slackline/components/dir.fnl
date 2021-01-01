(module slackline.components.dir
  {:require {: r
             : utils
             :t theme
             :hl slackline.highlight}

   :require-macros [macros]})

(defn render-in-context []
  (let [bufname (utils.fn.bufname "%")
        unlisted? (= (utils.fn.buflisted bufname) 0)
        wd (utils.fn.expand "%:.")
        pd (utils.fn.expand "%:.:h:t")
        fln (utils.fn.expand "%:t")]
    (if
      unlisted? ""
      (< (length wd) 14)  wd
      (.. "../" pd "/" fln))))

(defn- render-dir [{: get-current-color : active}]
  (let [name (or (get-current-color) "")]
    (..
      (if active
        (.. (hl.hl-comp (.. name " to dir")) "")
        "")
      "%#StatusLine" (if active "" "NC") "#"
      "%{ " (utils.viml->luaexp *module-name* (sym->name render-in-context)) "} ")))

(def- dir-get-colors #[{:bg t.c.bglighter :name :dir}
                       {:bg t.c.bglight :name :dirinactive}])
(defn dir-get-current-color [active] (if active #:dir #:dirinactive))

(defn main [child? args]
  (let [{: active : get-colors : get-current-color} (or args {:get-colors r.noop :get-current-color r.noop})
        child (if (r.function? child?) child? r.noop)]
    {:name :dir
     :render render-dir
     :next (child {: active :get-colors dir-get-colors :get-current-color (dir-get-current-color active)})
     :props {: get-current-color : active}
     :init
     (fn []
       (->>
         (get-colors)
         (r.for-each
           (fn [{: name : bg}]
             (hl.add-group (.. name " to dir") bg t.c.bglighter)))))}))
