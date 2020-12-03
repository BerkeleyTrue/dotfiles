(module plugins.emmet
  {:require {r r
             utils utils}})

(defn main []
  (utils.set-nvim-g! {:user_emmet_install_global 0
                      :user_emmet_settings {:html {:quote_char "'"}
                                            :jsx {:quote_char "'"}
                                            :javascript.jsx {:extends "jsx"}}}))
