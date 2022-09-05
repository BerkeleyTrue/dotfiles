(module plugins.telescope
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils

    ag plugins.telescope.silver-searcher}
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
      {:n
       {:q (. actions :close)}}}}))

(defn setup-keymaps []
  (utils.noremap :<leader>gf (utils.cviml->lua :telescope.builtin :git_files))
  (utils.noremap :<leader>ff (utils.cviml->lua :telescope.builtin :find_files))
  (utils.noremap :<leader>fk (utils.cviml->lua :telescope.builtin :keymaps))
  (utils.noremap :z= (utils.cviml->lua :telescope.builtin :spell_suggest)))

(defn setup-commands []
  (command! :HLights (utils.viml->lua :telescope.builtin :highlights))
  (command! :MMaps (utils.viml->lua :telescope.builtin :keymaps))
  (command! :BBuffs (utils.viml->lua :telescope.builtin :buffers))
  (command! :CodeActions (utils.viml->lua :telescope.builtin :lsp_code_actions))
  (command! :HHelp (utils.viml->lua :telescope.builtin :help_tags))
  (command! :RRegisters (utils.viml->lua :telescope.builtin :registers)))

(defn main []
  (when-let [telescope (md.prequire :telescope)]
    (let [sorters (md.prequire :telescope.sorters)
          previewers (md.prequire :telescope.previewers)
          builtins (md.prequire :telescope.builtin)
          actions (md.prequire :telescope.actions)]

      (setup {: telescope : sorters : previewers : actions})
      (setup-keymaps)
      (setup-commands)
      (ag.main))))
