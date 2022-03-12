(module plugins.zen
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (when-let [zen (md.prequire :zen-mode)]
    (zen.setup)))
