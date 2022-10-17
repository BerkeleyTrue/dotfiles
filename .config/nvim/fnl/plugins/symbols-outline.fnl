(module plugins.symbols-outline
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn main []
  (when-let [so (md.prequire :symbols-outline)]
    (so.setup)))
