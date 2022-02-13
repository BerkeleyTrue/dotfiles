(module plugins.vimsence
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (utils.set-nvim-g!
    {
     ; :vimsence_client_id "439476230543245312"
     :vimsence_small_text "Neovim"
     :vimsence_small_image "neovim"
     :vimsence_editing_details "Wizarding: {}"
     :vimsence_editing_state "On: {}"
     :vimsence_file_explorer_text "neo-tree"
     :vimsence_file_explorer_details "Looking for flies."
     ; :vimsence_custom_icons {:filetype :iconname}
     :vimsence_discord_flatpak 1}))
