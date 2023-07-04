(module plugins.emmet
  {:require {r r
             utils utils}
   :require-macros [macros]})


(defn add-emmet []
  (utils.ex.EmmetInstall)
  (utils.imap :<leader><tab> "<plug>(emmet-expand-abbr)" {:buffer true})
  (utils.inoremap "/<leader><tab>" "<esc>:call emmet#expandAbbr(0,'')<cr>h:call emmet#splitJoinTag()<cr>wwi" {:buffer true}))


(def filetypes [:css :html :javascript :jsx :markdown :typescriptreact :xml])

(defn main []
  (utils.set-nvim-g!
    {:user_emmet_install_global 0
     :user_emmet_settings
     {:html {:quote_char "'"}
      :jsx {:quote_char "'"}
      :javascript.jsx {:extends "jsx"}}})

  (let [(ok res) (pcall utils.ex.packadd :emmet-vim)]
    (if (not ok) (print (.. "Could not load emmet: " res))
      (utils.augroup
        :emmet-au
        [{:event :FileType
          :pattern filetypes
          :cmd (.. ":" (utils.viml->lua *module-name* (sym->name add-emmet)))}]))))
