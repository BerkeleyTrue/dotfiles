(module slackline.components.modified
  {:require {: r
             : utils
             :t theme
             :hl slackline.highlight}
   :require-macros [macros]})


(defn render-in-context []
  (let [modified? (->
                    (. utils.g :actual_curbuf)
                    (or  "")
                    (utils.fn.bufname)
                    (utils.fn.getbufvar "&mod")
                    (= 1))]

    (if modified? "   " "")))

(defn- render-modified [props?]
  (let [{: active} (or props? {})]
    (..
      (hl.hl-comp :modified)
      (..
        "%{"
        (utils.viml->luaexp *module-name* (sym->name render-in-context))
        "}")
      :%#StatusLine#)))


(defn main [child? args]
  (let [{: active} (or args {})
        child (if (r.function? child?) child? r.noop)]
    {:name :modified
     :render render-modified
     :props args
     :next (child args)
     :init (fn [] (hl.add-group :modified t.c.red))}))
