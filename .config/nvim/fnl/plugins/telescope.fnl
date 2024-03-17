(module plugins.telescope
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    hl utils.highlights}
   require
   {alt plugins.telescope.alternate
    ag plugins.telescope.silver-searcher
    tabs plugins.telescope.tabs
    todos plugins.telescope.todos}
   require-macros [macros]})

(defn- setup [{: telescope : previewers : sorters : actions}]
  (telescope.setup
    {:defaults
     {:borderchars ["─" "│" "─" "│" "╭" "╮" "╯" "╰"]
      :prompt_prefix "   "
      :selection_caret "> "
      :entry_prefix "  "
      :path_display [:truncate]

      :layout_strategy :vertical

      :layout_config
      {:horizontal
       {:prompt_position :bottom
        :height 0.8
        :width 0.8
        :preview_cutoff 120}

       :vertical
       {:height 0.9
        :preview_cutoff 40
        :prompt_position :top
        :mirror true
        :width 0.6}}

      :file_sorter (. sorters :get_fuzzy_file)
      :generic_sorter (. sorters :get_generic_fuzzy_sorter)
      :file_ignore_patterns [:node_modules :COMMIT_EDITMSG]
      :mappings
      {:n {:qq (. actions :close)}
       :i {:qq (. actions :close)}}}}))

(defn setup-keymaps []
  (nnoremap :<leader>fg (utils.cviml->lua :telescope.builtin :git_files) {:silent true :desc "Open git files search"})
  (nnoremap :<leader>ff (utils.cviml->lua :telescope.builtin :find_files) {:silent true :desc "Open files search"})
  (nnoremap :<leader>fk (utils.cviml->lua :telescope.builtin :keymaps) {:silent true :desc "Open keymaps search"})
  (nnoremap :<leader>fo (utils.cviml->lua :telescope.builtin :oldfiles {:args "{cwd_only = true}"}) {:silent true :desc "Open old files search"})
  (nnoremap :<leader>fh (utils.cviml->lua :telescope.builtin :help_tags) {:silent true :desc "Open help tags search"})
  (nnoremap :<leader>fl (utils.cviml->lua :telescope.builtin :highlights) {:silent true :desc "Open highlights search"})
  (nnoremap :<leader>fr (utils.cviml->lua :telescope.builtin :resume) {:silent true :desc "Resume last search "})
  (nnoremap :<leader>ft (utils.cviml->lua :telescope.builtin :live_grep) {:silent true :desc "Open live text search"})
  (nnoremap :<leader>fm (utils.cviml->lua :telescope.builtin :marks) {:silent true :desc "Open marks search"})
  (nnoremap :<leader>fj (utils.cviml->lua :telescope.builtin :jumplist) {:silent true :desc "Open jumplist search"})
  (nnoremap :z=         (utils.cviml->lua :telescope.builtin :spell_suggest) {:silent true :desc "Open spell suggest search"}))

(defn setup-commands []
  (command! :HLights (utils.viml->lua :telescope.builtin :highlights))
  (command! :MMaps (utils.viml->lua :telescope.builtin :keymaps))
  (command! :CodeActions (utils.viml->lua :telescope.builtin :lsp_code_actions))
  (command! :HHelp (utils.viml->lua :telescope.builtin :help_tags))
  (command! :OOldFiles (utils.viml->lua :telescope.builtin :oldfiles {:args "{cwd_only = true}"}))
  (command! :FFiles (utils.viml->lua :telescope.builtin :find_files {:args "{find_command = rg, hidden = true, files = true}"})))


(defn main []
  (hl.link! :TelescopeBorder :FloatBorder)
  (when-let [telescope (md.prequire :telescope)]
    (let [sorters (md.prequire :telescope.sorters)
          previewers (md.prequire :telescope.previewers)
          builtins (md.prequire :telescope.builtin)
          actions (md.prequire :telescope.actions)]

      (setup {: telescope : sorters : previewers : actions})
      (setup-keymaps)
      (setup-commands)
      (ag.main)
      (alt.main telescope)
      (tabs.main telescope)
      (todos.main telescope))))
