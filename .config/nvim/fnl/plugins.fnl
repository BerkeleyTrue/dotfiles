(module plugins
  {require {: r}
   require-macros [macros]})

(def- plugins
  [:ale
   :aniseed
   :autopairs
   :barbar
   :beacon
   :better-whitespace
   :colorizer
   :comments
   :completion
   :conjure
   :corpus
   :easy-align
   :emmet
   :fzf
   :hop
   :lspconfig
   :lspfuzzy
   :lualine
   :luasnip
   :markdown
   :mini
   :multi-cursor
   :neogen
   :neotree
   :nerd-commenter
   :null-ls
   :package-info
   :runtime-utils
   :sandwich
   :sexp
   :startify
   :telescope
   :terraform
   :tree-sitter
   :vimsence
   :vimtex])

(defn main []
  (->>
   plugins
   (r.map #(.. *module-name* "." $1))
   (r.forEach #(run-main $1))))

(comment (main))
