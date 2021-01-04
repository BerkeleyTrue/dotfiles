(module plugins.completion
  {:require {: r
             : utils
             :conjure completion-sources.conjure
             :tenk completion-sources.tenk}

   :require-macros [macros]})

(defn attach-complete []
  (let [completion (require :completion)]
    (completion.on_attach {:confirm_key ""})))

(defn main []
  (utils.set-nvim-g!
    {:completion_enable_snippet :UltiSnips
     :completion_sorting :length
     :completion_matching_smart_case true
     :completion_auto_change_source true
     :completion_matching_strategy_list [:exact]
     :completion_chain_complete_list
     {:default
      {:default
       [{:complete_items [:lsp :snippet :path :tenk :buffers :tmux]}
        {:complete_items [:lsp]}
        {:complete_items [:snippet]}
        {:complete_items [:buffers]}
        {:mode :<C-p>}
        {:mode :<C-n>}]
       :comment []}
      :lua
      {:default
       [{:complete_items [:ts :buffers]}]}
      :fennel
      {:default
       [{:complete_items [:ts :conjure :snippet :buffer :tmux :tenk]}
        {:complete_items [:ts :conjure :snippet]}
        {:complete_items [:tenk]}]
       :string
       [{:complete_items [:path :tenk]}]
       :comment
       [{:complete_items [:path :tenk]}]}
      :javascript
      {:default
       [{:complete_items [:ts :lsp :snippet :path :tenk :buffer :tmux]}
        {:complete_items [:ts]}
        {:complete_items [:lsp]}
        {:complete_items [:snippet]}
        {:complete_items [:buffers]}
        {:complete_items [:tmux]}]
       :string
       [{:complete_items [:path :tenk]}
        {:complete_items [:buffers]}]
       :comment
       [{:complete_items [:path :tenk]}
        {:complete_items [:buffers]}]}}})



  (let [(ok res) (pcall utils.ex.packadd :completion-nvim)]
    (if (not ok) (print "Could not load completion.nvim: " res)
      (utils.augroup :completion-au
                     [{:event :BufEnter
                       :pattern :*
                       :cmd (utils.viml->lua *module-name* (sym->name attach-complete))}])
      ; use <c-j> to switch to previous completion)
      (utils.imap :<c-j> "<Plug>(completion_next_source)")
      ; use <c-k> to switch to next completion))))
      (utils.imap :<c-k> "<Plug>(completion_prev_source)"))))
