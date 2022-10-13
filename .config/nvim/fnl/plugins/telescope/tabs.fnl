(module plugins.telescope.tabs
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main [telescope]
  (when-let [tabs (md.prequire :telescope-tabs)]
    (tabs.setup {})
    (telescope.load_extension :telescope-tabs)))
