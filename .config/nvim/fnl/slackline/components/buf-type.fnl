(module slackline.components.buf-type
  {:require {: r
             : utils
             :t theme
             :hl slackline.highlight}
   :require-macros [macros]})

(def- buf-icon
  {:help        "  "
   :defx        "  "
   :denite      "  "
   :vim-plug    "  "
   :vista       " 識"
   :vista_kind  "  "
   :dbui        "  "
   :magit       "  "
   :LuaTree     "  "})

(defn render-in-context []
  (let [ft (. utils.bo :filetype)
        icon (. buf-icon ft)]
    (if icon
      (..
        icon)
      "")))

(defn- render-icon [args]
  (let [{: active} (or args {})]
    (utils.print "active" active)
    (..
      (hl.hl-comp :render-icon)
      (if active (..  "%{" (utils.viml->luaexp *module-name* (sym->name render-in-context)) "}") ""))))


(defn main [child? args]
  (let [{: active} (or args {})
        child (if (r.function? child?) child? r.noop)]
    {:name :buf-type
     :init (fn [] (hl.add-group "buftype" t.c.cyan))
     :props {: active}
     :next (child args)
     :render render-icon}))
