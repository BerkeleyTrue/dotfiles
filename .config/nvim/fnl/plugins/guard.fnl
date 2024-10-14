(module plugins.guard
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    guard guard
    ft guard.filetype}
   require-macros [macros]})

(def- prettier-fts
  (->>
    ["javascript"
     "typescript"
     "typescriptreact"
     "html"
     "css"
     "scss"
     "less"
     "html"
     "json"
     "jsonc"
     "yaml"
     "markdown"
     "markdown.corpus"
     "solidity"
     "graphql"]
    (r.join ",")))

(defn main []
  (->
    (ft prettier-fts)
    (: :fmt :prettier)) ; requires prettier for formatting

  (->
    (ft :nix)
    (: :fmt {:cmd "alejandra" :args [:-c :--quiet] :stdin true})) ; requires alejandra for formatting

  (->
    (ft "bash,sh,zsh")
    (: :fmt :shfmt)) ; requires shfmt for formatting

  (->
    (ft :purescript)
    (: :fmt {:cmd "purs-tidy" :args [:format] :stdin true})) ; requires purs-tidy for formatting

  (->
    (ft :templ)
    (: :fmt {:cmd "templ" :args [:fmt] :stdin true})) ; requires templ for formatting

  (->
    (ft :go)
    (: :fmt :lsp)
    (: :append :golines))
  (->
    (ft :haskell)
    (: :fmt :lsp))
  (->
    (ft :sql)
    (: :fmt :sql-formatter))
  (->
    (ft "clojure,clojurescript")
    (: :fmt :lsp))
  (->
    (ft :slint)
    (: :fmt :lsp))

  (->
    (ft :rust)
    (: :fmt :lsp))

  (g! guard_config {:fmt_on_save false})

  (augroup
    :MarkdownGuard
    ; add zf to format markdown
    {:event [:BufReadPre]
     :pattern :*.md
     :cmd "nnoremap <buffer> <silent> zf <CMD>Guard fmt<CR>"}
    {:event [:BufReadPre]
     :pattern :*.sql
     :cmd "nnoremap <buffer> <silent> zf <CMD>Guard fmt<CR>"}))
