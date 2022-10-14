(module plugins.glow
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn main []
  (when-let [glow (md.prequire :glow)]
    (glow.setup {})))
