(module plugins.treej
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn init []
  (noremap :<leader>m :<CMD>TSJToggle<CR> {:silent true}))

(defn main []
  (let [treesj (md.prequire :treesj)]
    (treesj.setup {:use_default_keymaps false})))
