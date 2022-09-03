(module plugins.mini
  {require
   {a aniseed.core
    r r
    md utils.module
    hl utils.highlights
    utils utils}
   require-macros [macros]})

(defn main []
  (when-let [indent (md.prequire :mini.indentscope)]
    (indent.setup)
    (hl.link! :MiniIndentscopeSymbol :BerksSubtle)))
