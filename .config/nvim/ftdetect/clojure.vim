" no autogroup needed as the file is sourced in an autogroup by vim
autocmd BufNewFile,BufRead *.clj,*.cljs,*.edn,*.cljx,*.cljc,{build,profile}.boot setlocal filetype=clojure
