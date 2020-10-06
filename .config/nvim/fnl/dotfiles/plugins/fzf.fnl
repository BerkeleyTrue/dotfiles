(module dotfiles.plugins.fzf
  {:require {a aniseed.core
             nvim aniseed.nvim
             woating dotfiles.plugins.woating
             utils dotfiles.utils}})



(a.assoc nvim.g :fzf_layout {:window (utils.viml->lua :dotfiles.plugins.woating :create-woating)})
