(module plugins.surround
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (when-let [surround (md.prequire :nvim-surround)]
    (surround.setup {})))
