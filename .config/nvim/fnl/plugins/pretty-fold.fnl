(module plugins.pretty-fold
  {require
   {md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (when-let [pretty-fold (md.packadd-n-require :pretty-fold.nvim :pretty-fold)]
    (pretty-fold.setup {})
    (: (md.prequire :pretty-fold.preview) :setup)))
