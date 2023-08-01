(module plugins.notify
  {require
   {a aniseed.core
    anenv plugins.aniseed
    md utils.module
    utils utils}
   require-macros [macros]})


(def config
  {:background_colour "#000000"})

(defn main []
  (when-let [notify (md.prequire :notify)]
    (notify.setup config)))
