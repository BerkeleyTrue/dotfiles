(module plugins.fzf
  {:require {a aniseed.core
             str aniseed.string
             nvim aniseed.nvim
             nutil aniseed.nvim.util
             woating plugins.woating
             utils utils}})

(a.assoc nvim.g :fzf_layout {:window (utils.viml->lua :plugins.woating :create-woating)})

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

(defn ag-in-fzf-from-cwb [...]
  (let [dir (nvim.fn.expand "%:p:h")]
    (ag-in-fzf ... dir)))

(do
  (nvim.ex.augroup :fzf-ag)
  (nvim.ex.autocmd_)
  (nvim.ex.autocmd (..
                       "VimEnter * command! -nargs=* -complete=dir Ag "
                       (utils.viml->lua :plugins.fzf :ag-in-fzf {:args "<f-args>"})))
  (nvim.ex.autocmd (..
                       "VimEnter * command! -nargs=* -complete=dir Agdot "
                       (utils.viml->lua :plugins.fzf :ag-in-fzf-from-cwb {:args "<f-args>"})))
  (nvim.ex.augroup :END)
  {:ag-in-fzf ag-in-fzf
   :ag-in-fzf-from-cwb ag-in-fzf-from-cwb})
