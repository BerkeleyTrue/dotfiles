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
       {:enabled false
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
       {:yaml false
        :markdown false
        :help false
        :gitcommit false
        :gitrebase false
        :hgcommit false
        :svn false
        :cvs false
        :. false}
       :copilot_node_command :node
       :server_opts_overrides {}})

    (let [cpsuggestions (md.prequire :copilot.suggestion)]
      (imap
        :<Right>
        (fn []
          (if (cpsuggestions.is_visible)
            (cpsuggestions.accept)
            (keys.feed :<Right> true)))
        {:silent true}))))
