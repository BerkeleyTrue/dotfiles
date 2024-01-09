" no autogroup needed as the file is sourced in an autogroup by vim
autocmd BufNewFile,BufRead *.clj,*.cljs,*.edn,*.cljx,*.cljc,{build,profile}.boot setlocal filetype=clojure
" check if file has shebang and if it is a bb script and set filetype to clojure
autocmd BufNewFile,BufRead * :lua if vim.regex("^#!.*bb"):match_str(vim.fn.getline(1)) then vim.bo.filetype = "clojure" end
