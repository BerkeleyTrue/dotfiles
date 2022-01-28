(module plugins.telescope
  {:require {a aniseed.core
             : utils
             : telescope
             sorters telescope.sorters}})



(defn- setup [telescope]
  (telescope.setup
    {:defaults {}
     :borderchars {
                   1 ["─" "│" "─" "│" "╭" "╮" "╯" "╰"]
                   :preview ["─" "│" "─" "│" "╭" "╮" "╯" "╰"]}

     :sorting_strategy "descending"
     :prompt_position "bottom"
     :color_devicons true
     :file_sorter sorters.get_fzy_sorter}))

(defn setup-keymaps []
  (utils.noremap :<leader>gf (utils.cviml->lua :telescope.builtin :git_files))
  (utils.noremap :<leader>ff (utils.cviml->lua :telescope.builtin :find_files))
  (utils.noremap :<leader>fk (utils.cviml->lua :telescope.builtin :keymaps))
  (utils.ex.command_ :HLights (utils.viml->lua :telescope.builtin :highlights))
  (utils.ex.command_ :NMaps (utils.viml->lua :telescope.builtin :keymaps))
  (utils.ex.command_ :BBuffs (utils.viml->lua :telescope.builtin :buffers))
  (utils.ex.command_ :CodeActions (utils.viml->lua :telescope.builtin :lsp_code_actions)))

(defn main []
  (setup telescope)
  (setup-keymaps))
