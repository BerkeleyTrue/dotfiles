(module plugins.vimtex
  {require {utils utils}})

(defn main []
  (utils.set-nvim-g!
    {:vimtex_fold_enabled 0
     :tex_conceal 1
     :vimtex_view_method :zathura
     :tex_flavor :plain}))
