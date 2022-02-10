(module plugins.lspkind
  {require
   {: utils
    md utils.module}
   require-macros [macros]})


(defn main []
  (when-let [lspkind (md.packadd-n-require :lspkind-nvim :lspkind)]
    lspkind))
