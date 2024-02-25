(module plugins.treej
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    treesj treesj
    langs treesj.langs
    mini mini.splitjoin}
   require-macros [macros]})

(defn get-lang [node]
  (let [[row col] (n win-get-cursor 0)
        range [(- row 1) col
               (- row 1) col]
        bufnr (n get_current_buf)
        lang (vim.treesitter.language.get_lang (bo ft bufnr))]

    (when-let [(ok? parser) (pcall vim.treesitter.get_parser bufnr lang)
               current-tree (parser:language_for_range range)]
      (current-tree:lang))))

(defn handle-toggle []
  (when-let [lang (get-lang)
             _ (a.println :lang lang)
             lang (. langs.presets lang)]
    (a.println :lang2 lang)
    (if lang
      (treesj.toggle)
      (mini.toggle))))

(defn init []
  (noremap :gJ handle-toggle {:silent true :desc "Treesj SplitJoin toggle"}))

(defn main []
  (treesj.setup
    {:use_default_keymaps false
     :langs {:templ langs.go}}))
