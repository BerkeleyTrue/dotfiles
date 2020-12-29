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

(defn render-ale []
  (..
    "%="
    (hl.hl-comp :StatusToYellow) ""
    "%#" :BerksYellow "#"
    "%1(%{" (utils.viml->luaexp *module-name* (sym->name render-in-context) "\"warning\"") "}%)"
    (hl.hl-comp :YellowToRedInverse) ""
    "%#" :BerksRedInverse "#"
    "%1(%{" (utils.viml->luaexp *module-name* (sym->name render-in-context) "\"err\"") "}%)"))

(defn main [child? args]
  (let [{: active} (or args {})
        child (if (r.function? child?) child? r.noop)]
    {:name :ale
     :render render-ale
     :next (child args)
     :init
     (fn []
       (hl.add-group :YellowToRedInverse t.c.red t.c.bg)
       (hl.add-group :StatusToYellow t.c.bg t.c.bglighter))}))
