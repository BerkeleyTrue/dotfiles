(module plugins.telescope
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    hl utils.highlights
    telescope telescope
    builtin telescope.builtin}
   require
   {ag plugins.telescope.silver-searcher
    tabs plugins.telescope.tabs
    todos plugins.telescope.todos}
   require-macros [macros]})

(defn- setup [{: sorters : actions}]
  (telescope.setup
    {:defaults
     {:borderchars ["─" "│" "─" "│" "╭" "╮" "╯" "╰"]
      :prompt_prefix "   "
      :selection_caret "> "
      :entry_prefix "  "
      :path_display [:truncate]

      :layout_strategy :vertical

      :layout_config {:horizontal
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

      :mappings {:n {:qq (. actions :close)}
                 :i {:qq (. actions :close)}}}

     :pickers {:find_files {:hidden false}
               :oldfiles {:cwd_only true}}}))

(defn setup-keymaps []
  (nnoremap :<leader>fg (utils.cviml->lua :telescope.builtin :git_files) {:silent true :desc "Open git files search"})
  (nnoremap :<leader>ff (utils.cviml->lua :telescope.builtin :find_files) {:silent true :desc "Open files search"})
  (nnoremap :<leader>fk (utils.cviml->lua :telescope.builtin :keymaps) {:silent true :desc "Open keymaps search"})
  (nnoremap :<leader>fo (utils.cviml->lua :telescope.builtin :oldfiles) {:silent true :desc "Open old files search"})
  (nnoremap :<leader>fh (utils.cviml->lua :telescope.builtin :help_tags) {:silent true :desc "Open help tags search"})
  (nnoremap :<leader>fl (utils.cviml->lua :telescope.builtin :highlights) {:silent true :desc "Open highlights search"})
  (nnoremap :<leader>fr (utils.cviml->lua :telescope.builtin :resume) {:silent true :desc "Resume last search "})
  (nnoremap :<leader>ft (utils.cviml->lua :telescope.builtin :live_grep) {:silent true :desc "Open live text search"})
  (nnoremap :<leader>fm (utils.cviml->lua :telescope.builtin :marks) {:silent true :desc "Open marks search"})
  (nnoremap :<leader>fj (utils.cviml->lua :telescope.builtin :jumplist) {:silent true :desc "Open jumplist search"})
  (nnoremap :<leader>fb (utils.cviml->lua :telescope.builtin :buffers) {:silent true :desc "Open buffers search"})
  (nnoremap :z=         (utils.cviml->lua :telescope.builtin :spell_suggest) {:silent true :desc "Open spell suggest search"}))

(defn setup-commands []
  (command! :HLights (utils.viml->lua :telescope.builtin :highlights))
  (command! :MMaps (utils.viml->lua :telescope.builtin :keymaps))
  (command! :HHelp (utils.viml->lua :telescope.builtin :help_tags))
  (command! :OOldFiles (utils.viml->lua :telescope.builtin :oldfiles))
  (command! :Files #(builtin.find_files {:hidden true :no_ignore true :no_ignore_parent true})))


(defn main []
  (hl.link! :TelescopeBorder :FloatBorder)
  (let [sorters (md.prequire :telescope.sorters)
        builtins (md.prequire :telescope.builtin)
        actions (md.prequire :telescope.actions)]

    (setup {: sorters : actions})
    (setup-keymaps)
    (setup-commands)
    (ag.main)
    (tabs.main telescope)
    (todos.main telescope)))
