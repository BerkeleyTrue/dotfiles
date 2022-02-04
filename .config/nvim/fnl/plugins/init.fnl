(module plugins
  {:require {: r}
   :require-macros [macros]})

(defn main []
  (->>
    [:ale
     :beacon
     :better-whitespace
     :colorizer
     :completion
     :conjure
     :delimit-mate
     :easy-align
     :easy-motion
     :emmet
     :fzf
     :lspconfig
     :lspfuzzy
     :lspkind
     :markdown
     :mta
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
     :ultisnips
     :vimtex]
    (r.map #(.. *module-name* "." $1))
    (r.forEach #(run-main $1))))

(comment (main))
