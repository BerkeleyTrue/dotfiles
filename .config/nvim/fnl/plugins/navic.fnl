(module plugins.navic
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (when-let [navic (md.prequire :nvim-navic)]
    (navic.setup {})))
