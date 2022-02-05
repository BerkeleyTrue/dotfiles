(module plugins.autopairs
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (when-let [ap (md.packadd-n-require :nvim-autopairs)]
    (ap.setup {})))
