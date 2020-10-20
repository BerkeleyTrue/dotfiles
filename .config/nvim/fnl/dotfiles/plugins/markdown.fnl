(module dotfiles.plugins.markdown
  {:require {a aniseed.core
             nvim aniseed.nvim}})


(set nvim.g.vim_markdown_conceal 0)
(set nvim.g.vim_markdown_new_list_item_indent 2)
(set nvim.g.vim_markdown_no_extensions_in_markdown 1)
(set nvim.g.vim_markdown_frontmatter 1)
