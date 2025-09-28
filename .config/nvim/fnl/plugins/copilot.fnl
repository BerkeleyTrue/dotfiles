(module plugins.copilot
  {autoload 
   {copilot copilot
    suggest copilot.suggestion}
   require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    keys utils.keys}
   require-macros [macros]})

(defn main []
  (copilot.setup
    {:panel
     {:enabled false}

     :suggestion
     {:enabled true
      :auto_trigger true
      :debounce 450
      :keymap
      {:accept false
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
     :server_opts_overrides {}}

    (imap
      :<Right>
      (fn copilot-accept []
        (if (suggest.is_visible)
          (suggest.accept)
          (keys.feed :<Right> true)))
      {:silent true})))
