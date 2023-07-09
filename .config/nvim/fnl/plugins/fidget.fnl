(module plugins.fidget
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main [])
  ; (when-let [fidget (md.prequire :fidget)]
  ;   (fidget.setup
  ;     {:sources
  ;      {:null-ls
  ;       {:ignore true}}
  ;      :text
  ;      {:spinner :moon}
  ;      :legacy true})))
