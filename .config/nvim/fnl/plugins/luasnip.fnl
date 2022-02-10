(module plugins.luasnip
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (when-let [luasnip (md.packadd-n-require luasnip)]
    (print luasnip)))
