(module plugins.neogen
  {require
   {md utils.module
    utils utils}
   require-macros [macros]})


(defn main []
  (when-let [neogen (md.prequire :neogen)]
    (neogen.setup {:enabled true})))
