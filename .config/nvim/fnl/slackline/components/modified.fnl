(module slackline.components.modified
  {require
   {: r
    : utils
    :t theme
    :hl slackline.highlight}
   require-macros [macros]})

(defn diviser-in-context []
  (let [modified? (->
                    (. utils.g :actual_curbuf)
                    (or  "")
                    (utils.fn.bufname)
                    (utils.fn.getbufvar "&mod")
                    (= 1))]

    (if modified? " " "")))

(defn render-in-context []
  (let [modified? (->
                    (. utils.g :actual_curbuf)
                    (or  "")
                    (utils.fn.bufname)
                    (utils.fn.getbufvar "&mod")
                    (= 1))]

    (if modified? " " "")))

(defn end-diviser-in-context []
  (let [modified? (->
                    (. utils.g :actual_curbuf)
                    (or  "")
                    (utils.fn.bufname)
                    (utils.fn.getbufvar "&mod")
                    (= 1))]

    (if modified? "" "")))

(defn- render-modified [props?]
  (let [{: active : get-current-color} (or props? {})
        name (get-current-color)]
    (..
      (hl.hl-comp (.. name " to modified"))
      "%{" (utils.viml->luaexp *module-name* (sym->name diviser-in-context)) "}"
      (hl.hl-comp :modified)
      "%{" (utils.viml->luaexp *module-name* (sym->name render-in-context)) "}"
      (hl.hl-comp (.. "mod end row" (if active "" "nc")))
      "%{" (utils.viml->luaexp *module-name* (sym->name end-diviser-in-context)) "}"
      "%#StatusLine" (if active "" "NC") "#")))


(defn main [child? args]
  (let [{: active : get-colors} (or args {})
        child (if (r.function? child?) child? r.noop)]
    {:name :modified
     :render render-modified
     :props args
     :next (child args)
     :init
     (fn []
       (hl.add-group :modified t.c.red)
       (hl.add-group "mod end row" t.c.bg t.c.bglighter)
       (hl.add-group "mod end row nc" t.c.bg t.c.bglight)
       (->>
         (get-colors)
         (r.for-each
            (fn [{: name : bg}]
              (hl.add-group (.. name " to modified") bg)))))}))
