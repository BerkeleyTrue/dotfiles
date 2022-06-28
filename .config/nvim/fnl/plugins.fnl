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
   :vimsence
   :easy-align
   :easy-motion
   :emmet
   :fzf
   :lualine
   :luasnip
   :lspconfig
   :lspfuzzy
   :markdown
   :mini
   :multi-cursor
   :neogen
   :neotree
   :nerd-commenter
   :null-ls
   :sandwich
   :sexp
   :startify
   :package-info
   :pretty-fold
   :runtime-utils
   :telescope
   :terraform
   :tree-sitter
   :vimtex])

(defn main []
  (->>
   plugins
   (r.map #(.. *module-name* "." $1))
   (r.forEach #(run-main $1))))

(comment (main))
