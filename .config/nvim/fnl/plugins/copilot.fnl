(module plugins.copilot
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    keys utils.keys}
   require-macros [macros]})

(defn main []
  (when-let [copilot (md.prequire :copilot)]
    (copilot.setup
      {:panel
       {:enabled true
        :auto_refresh false
        :keymap
        {:jump_prev "[["
         :jump_next "]]"
         :accept :<CR>
         :refresh :gr
         :open :<M-CR>}}
       :suggestion
       {:enabled true
        :auto_trigger true
        :debounce 250
        :keymap
        {:accept false
         :next "]]"
         :prev "[["
         :dismiss "<C-]>"}}
       :filetypes
       {:help false
        :gitcommit false
        :gitrebase false
        :hgcommit false
        :svn false
        :cvs false
        :. false}
       :copilot_node_command (. vim.g :copilot_node_command) ; provided through nix
       :server_opts_overrides {}})

    (let [cpsuggestions (md.prequire :copilot.suggestion)]
      (imap
        :<Right>
        (fn copilot-accept []
          (if (cpsuggestions.is_visible)
            (cpsuggestions.accept)
            (keys.feed :<Right> true)))
        {:silent true}))))
