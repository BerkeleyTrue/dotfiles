(module plugins.zenmode
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (let [zm (md.prequire :zen-mode)]
    (zm.setup
      {:plugins {:kitty {:enable true}}})))
