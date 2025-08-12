(module plugins.emmet
  {require
   {r r
    utils utils}
   require-macros [macros]})


(defn add-emmet []
  (cmd EmmetInstall)
  (imap :<leader><tab> "<plug>(emmet-expand-abbr)" {:buffer true})
  (inoremap "/<leader><tab>" "<esc>:call emmet#expandAbbr(0,'')<cr>h:call emmet#splitJoinTag()<cr>wwi" {:buffer true}))


(def filetypes
  [:css
   :html
   :javascript
   :jsx
   :markdown
   :jinja
   :templ
   :typescriptreact
   :xml])

(defn init []
  (utils.set-nvim-g!
    {:user_emmet_install_global 0
     :user_emmet_leader_key "<c-e>"
     :user_emmet_settings
     {:html {:quote_char "'"}
      :jsx {:quote_char "'"}
      :javascript.jsx {:extends "jsx"}}})
  (augroup
    :EmmetGroup
    {:event :FileType
     :pattern filetypes
     :callback add-emmet}))

(defn main [])
