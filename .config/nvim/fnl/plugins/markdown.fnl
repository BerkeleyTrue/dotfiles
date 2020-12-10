(module plugins.markdown
  {:require {utils utils}})


(defn main []
  (utils.set-nvim-g! {:vim_markdown_conceal 0
                      :vim_markdown_new_list_item_indent 2
                      :vim_markdown_folding_disabled 1
                      :vim_markdown_no_extensions_in_markdown 1
                      :vim_markdown_frontmatter 1}))
