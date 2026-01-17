(module plugins.tree-sitter-context
  {autoload
   {a aniseed.core
    r r
    context treesitter-context}
   require {}
   import-macros []
   require-macros [macros]})

(defn main []
  (context.setup {:enable true
                  :max_lines 10
                  :line_numbers false
                  :multiline_threshold 4}))
