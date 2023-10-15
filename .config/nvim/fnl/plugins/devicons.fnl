(module plugins.devicons
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn main []
  (when-let [devicons (md.prequire :nvim-web-devicons)]
    (devicons.setup
      {:override_by_extension
       {:templ
        {:icon ""
         :color "#e5c890"
         :name "templ"}
        :gen.go
        {:icon ""
         :color "#ea999c"
         :name "sqlc"}}})))
