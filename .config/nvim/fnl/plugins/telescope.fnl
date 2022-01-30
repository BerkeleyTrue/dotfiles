(module plugins.telescope
  {:require
   {a aniseed.core
    r r
    str aniseed.string
    md utils.module
    utils utils}
   :require-macros [macros]})

(defn live-grep [...]
  "Live grep search term."
  (if-let [builtins (md.prequire :telescope.builtin)]
    (let [query (r.join " " [...])
          opts {}]

      (when query (tset opts :default_text query))
      (builtins.live_grep opts))
    (vim.notify "no telescope.builtin was found")))

(defn live-grep-from-glob [...]
  "Live grep from a search term and a glob>
  (live-grep-glob \"a\" \"search\" \"string\" \"**/*.js\")"
  (if-let [builtins (md.prequire :telescope.builtin)]
    (let [args [...]
          n (length args)
          dirs (when (>= n 2)
                 (->> args
                   (a.last)
                   (#(utils.fn.glob $ 0 1))
                   (r.map (fn [path] (utils.fn.fnamemodify path ":p:h")))
                   (r.uniq)))

          query (->> args
                  ((if (= n 1) a.identity a.butlast))
                  (r.join " "))

          opts {}]

      (when query (tset opts :default_text query))
      (when dirs (tset opts :search_dirs dirs))
      (builtins.live_grep opts))
    (vim.notify "no telescope.builtin was found")))

(defn live-grep-from-cbd [...]
  "Live grep from the current buffer directory.
  (live-grep-from-cbd \"Foo bar\")"
  (if-let [builtins (md.prequire :telescope.builtin)]
    (let [path (utils.fn.expand "%:p:h")
          query (r.join " " [...])]
      (builtins.live_grep
        {:default_text query :cwd path}))
    (vim.notify "no telescope.builtin was found")))

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

(defn setup-commands [{: builtins}]
  (utils.ex.command_ :HLights (utils.viml->lua :telescope.builtin :highlights))
  (utils.ex.command_ :NMaps (utils.viml->lua :telescope.builtin :keymaps))
  (utils.ex.command_ :BBuffs (utils.viml->lua :telescope.builtin :buffers))
  (utils.ex.command_ :CodeActions (utils.viml->lua :telescope.builtin :lsp_code_actions))
  (utils.ex.command_ :Help (utils.viml->lua :telescope.builtin :help_tags))
  (utils.ex.command_
    "-nargs=*"
    :Ag
    (utils.viml->lua *module-name* (sym->name live-grep) {:args "<f-args>"}))
  (utils.ex.command_
    "-nargs=*"
    :Agdot
    (utils.viml->lua *module-name* (sym->name live-grep-from-cbd) {:args "<f-args>"}))
  (utils.ex.command_
    "-nargs=*"
    :Agglob
    (utils.viml->lua *module-name* (sym->name live-grep-from-glob) {:args "<f-args>"})))

(defn main []
  (when-let [telescope (md.packadd-n-require :telescope.nvim :telescope)]
    (let [sorters (md.prequire :telescope.sorters)
          previewers (md.prequire :telescope.previewers)
          builtins (md.prequire :telescope.builtin)]

      (setup {: telescope : sorters : previewers})
      (setup-keymaps)
      (setup-commands {: builtins}))))
