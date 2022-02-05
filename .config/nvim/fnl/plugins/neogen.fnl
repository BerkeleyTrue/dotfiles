(module plugins.neogen
  {require
   {md utils.module
    utils utils}
   require-macros [macros]})


(defn main []
  (when-let [neogen (md.packadd-n-require :neogen)]
    (neogen.setup {:enabled true})))
