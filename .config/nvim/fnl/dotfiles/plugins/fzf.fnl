(module dotfiles.plugins.fzf
  {:require {a aniseed.core
             str aniseed.string
             nvim aniseed.nvim
             nutil aniseed.nvim.util
             woating dotfiles.plugins.woating
             utils dotfiles.utils}})

(a.assoc nvim.g :fzf_layout {:window (utils.viml->lua :dotfiles.plugins.woating :create-woating)})

(defn ag-in-fzf [...]
  (let [args [...]
        n (length args)
        path (nvim.fn.fzf#shellescape (if (>= n 2) (a.last args) "./"))
        query (nvim.fn.fzf#shellescape (if (>= n 1)
                                           (->> args
                                               ((if (= n 1) a.identity a.butlast))
                                               (str.join " "))
                                           "^(?=.)"))
        command (.. query " " path)]


    (nvim.fn.fzf#vim#ag_raw command)))

(do
  (nvim.ex.augroup :fzf-ag)
  (nvim.ex.autocmd_)
  (nvim.ex.autocmd (..
                       "VimEnter * command! -nargs=* -complete=dir Ag "
                       (utils.viml->lua :dotfiles.plugins.fzf :ag-in-fzf {:args "<f-args>"})))
  (nvim.ex.augroup :END)
  {:ag-in-fzf ag-in-fzf})
