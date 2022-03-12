(module plugins.twilight
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})



(defn main []
  (when-let [twilight (md.prequire :twilight)]
    (twilight.setup)))
