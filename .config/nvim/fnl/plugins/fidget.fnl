(module plugins.fidget
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (: (md.prequire :fidget) :setup {}))
