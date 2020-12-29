(module slackline.components.buf-type
  {:require {: r
             : utils
             :t theme
             :hl slackline.highlight}
   :require-macros [macros]})

(def- icon-map
  {:help       "  "
   :defx       "  "
   :denite     "  "
   :vim-plug   "  "
   :vista      " 識"
   :vista_kind "  "
   :dbui       "  "
   :magit      "  "
   :gitcommit  "  "
   :LuaTree    "  "})

(defn render-in-context []
  (let [ft (->
             (. utils.g :actual_curbuf)
             (or  "")
             (utils.fn.bufname)
             (utils.fn.getbufvar "&ft"))
        icon (. icon-map ft)]
    (or icon "")))

(defn- render-icon [args]
  (let [{: active} (or args {})]
    (..
      (hl.hl-comp :render-icon)
      "%{" (utils.viml->luaexp *module-name* (sym->name render-in-context)) "}")))

(defn main [child? args]
  (let [{: active} (or args {})
        child (if (r.function? child?) child? r.noop)]
    {:name :buf-type
     :init (fn [] (hl.add-group "buftype" t.c.cyan))
     :props {: active}
     :next (child args)
     :render render-icon}))
