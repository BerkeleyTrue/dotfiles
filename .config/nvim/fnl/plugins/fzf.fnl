(module plugins.fzf
  {require
   {a aniseed.core
    str aniseed.string
    nvim aniseed.nvim
    nutil aniseed.nvim.util
    woating plugins.woating
    utils utils}})

(defn main []
  (a.assoc nvim.g :fzf_layout {:window (utils.viml->lua :plugins.woating :create-woating)}))
