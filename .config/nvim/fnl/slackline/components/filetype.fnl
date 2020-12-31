(module slackline.components.filetype
  {:require {: r
             : utils
             :t theme
             :hl slackline.highlight
             :icons nvim-web-devicons}
   :require-macros [macros]})

(defn render-in-context []
  (let [filetype (->
                   (. utils.g :actual_curbuf)
                   (or  "")
                   (utils.fn.bufname)
                   (utils.fn.getbufvar "&ft"))
        filename (utils.fn.expand "%:t:r")
        ext (utils.fn.expand "%:e")
        icon (icons.get_icon filename ext {:default true})]
    (.. " " filetype " " icon)))


(defn- render-dir [{: get-current-color : active}]
  (let [name (if active :Status :StatusNC)]
    (..
      "%="
      (.. (hl.hl-comp (.. name " to filetype")) "")
      (hl.hl-comp :filetype)
      "%{" (utils.viml->luaexp *module-name* (sym->name render-in-context)) "} ")))

(def- dir-get-colors #[{:bg t.c.comment :name :filetype}])
(def- dir-get-current-color #:filetype)

(defn main [child? args]
  (let [{: active : get-colors : get-current-color} (or args {:get-colors r.noop :get-current-color r.noop})
        child (if (r.function? child?) child? r.noop)]
    {:name :filetype
     :render render-dir
     :next (child {: active :get-colors dir-get-colors :get-current-color dir-get-current-color})
     :props {: get-current-color : active}
     :init
     (fn []
       (hl.add-group "Status to filetype" t.c.comment t.c.bglighter)
       (hl.add-group "StatusNC to filetype" t.c.comment t.c.bglight)
       (hl.add-group "filetype" t.c.orange t.c.comment)
       (->>
         (get-colors)
         (r.for-each
           (fn [{: name : bg}]
             (hl.add-group (.. name " to filetype") bg t.c.comment)))))}))
