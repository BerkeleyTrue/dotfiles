(module plugins.vimtex
  {require {utils utils}})

(defn main []
  (utils.set-nvim-g!
    {:vimtex_fold_enabled 1
     :tex_conceal 0
     :tex_flavor :plain}))
