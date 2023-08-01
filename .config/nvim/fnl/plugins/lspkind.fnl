(module plugins.lspkind
  {require
   {: utils
    md utils.module}
   require-macros [macros]})


(defn main []
  (when-let [lspkind (md.prequire :lspkind)]
    lspkind))
