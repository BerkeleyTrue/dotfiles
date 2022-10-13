(module plugins.telescope.alternate
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(def config
  {:presets [:nestjs]
   :mappings
   [{:pattern "fnl/(.*).fnl"
     :targets
     [{:template "lua/[1].lua"
       :label "Output"}]}

    {:pattern "lua/(.*).fnl"
     :targets
     [{:template "fnl/[1].lua"
       :label "Input"}]}]})


(defn main [telescope]
  (when-let [alt (md.prequire :telescope-alternate)]
    (alt.setup config)
    (telescope.load_extension :telescope-alternate)
    (command! :TAlt "Telescope telescope-alternate alternate_file")))
