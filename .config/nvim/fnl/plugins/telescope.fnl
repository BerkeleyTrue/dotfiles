(module plugins.telescope
  {:require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    live-grep plugins.telescope.live-grep}
   :require-macros [macros]})

(defn- setup [{: telescope : previewers : sorters}]
  (telescope.setup
    {:defaults
     {:borderchars ["─" "│" "─" "│" "╭" "╮" "╯" "╰"]
      :prompt_prefix "   "
      :selection_caret "> "
      :entry_prefix "  "
      :path_display [:truncate]
      :vimgrep_arguments
      [:ag
       :--vimgrep
       :--nocolor
       :--noheading
       :--smart-case
       :--column]

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
      :file_ignore_patterns [:node_modules]}}))

(defn setup-keymaps []
  (utils.noremap :<leader>gf (utils.cviml->lua :telescope.builtin :git_files))
  (utils.noremap :<leader>ff (utils.cviml->lua :telescope.builtin :find_files))
  (utils.noremap :<leader>fk (utils.cviml->lua :telescope.builtin :keymaps)))

(defn setup-commands []
  (utils.ex.command_ :HLights (utils.viml->lua :telescope.builtin :highlights))
  (utils.ex.command_ :NMaps (utils.viml->lua :telescope.builtin :keymaps))
  (utils.ex.command_ :BBuffs (utils.viml->lua :telescope.builtin :buffers))
  (utils.ex.command_ :CodeActions (utils.viml->lua :telescope.builtin :lsp_code_actions))
  (utils.ex.command_ :Help (utils.viml->lua :telescope.builtin :help_tags)))

(defn main []
  (when-let [telescope (md.packadd-n-require :telescope.nvim :telescope)]
    (let [sorters (md.prequire :telescope.sorters)
          previewers (md.prequire :telescope.previewers)
          builtins (md.prequire :telescope.builtin)]

      (setup {: telescope : sorters : previewers})
      (setup-keymaps)
      (setup-commands)
      (live-grep.main {: builtins}))))
