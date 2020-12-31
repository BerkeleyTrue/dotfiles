(module slackline.components.ale
  {:require {: r
             : utils
             :t theme
             :hl slackline.highlight}

   :require-macros [macros]})

(defn render-in-context [type]
  (if (> (utils.fn.exists ":ALELint") 0)
    (let [err? (= type :err)
          {:error err
           : style_error
           : total} (utils.fn.ale#statusline#Count (utils.fn.bufnr "%"))
          errs (+ err style_error)
          warns (- total errs)
          show? (if err? (> errs 0) (> warns 0))
          render (.. (if err? (.. "  " errs " ") (.. "  " warns " ")))]
      (if show? render ""))
    ""))

(defn render-ale [{: get-current-color}]
  (..
    (hl.hl-comp (.. (get-current-color) " to ale")) ""
    "%#" :BerksYellow "#"
    "%1(%{" (utils.viml->luaexp *module-name* (sym->name render-in-context) "\"warning\"") "}%)"
    (hl.hl-comp :YellowToRedInverse) ""
    "%#" :BerksRedInverse "#"
    "%1(%{" (utils.viml->luaexp *module-name* (sym->name render-in-context) "\"err\"") "}%)"))

(def- dir-get-colors #[{:name :ale :bg t.c.none}])
(def- dir-get-current-color #:ale)

(defn main [child? args]
  (let [{: active : get-colors : get-current-color} (or args {:get-colors r.noop :get-current-color r.noop})
        child (if (r.function? child?) child? r.noop)]
    {:name :ale
     :render render-ale
     :next (child {: active :get-colors dir-get-colors :get-current-color dir-get-current-color})
     :props {: get-current-color : active}
     :init
     (fn []
       (hl.add-group :YellowToRedInverse t.c.red t.c.bg)
       (->>
         (get-colors)
         (r.for-each
           (fn [{: name : bg}]
             (hl.add-group (.. name " to ale") t.c.bg bg)))))}))
