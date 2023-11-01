(module plugins.whichkey
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (when-let [wk (md.prequire :which-key)]
    (wk.setup
      {:plugins
       {:spelling {:enabled false}}})))
