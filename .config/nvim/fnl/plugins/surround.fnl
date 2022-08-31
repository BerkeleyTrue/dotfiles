(module plugins.surround
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (when-let [surround (md.packadd-n-require :nvim-surround)]
    (surround.setup {})))
