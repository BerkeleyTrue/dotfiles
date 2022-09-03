(module plugins.colorizer
  {require
   {a aniseed.core
    md utils.module}
   require-macros [macros]})

(defn main []
  (when-let [colorizer (md.prequire :colorizer)]
    (colorizer.setup [:css :javascript :lua :fennel :typescript]))) ; kinda slow with large files, restrict scope
