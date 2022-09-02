(module plugins
  {require {r r}
   require-macros [macros]})

(def- plugins
  [:accents
   :completion
   :hlslens
   :hop
   :lspconfig
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
   :sexp
   :scroll-fix
   :surround
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
