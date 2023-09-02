(module plugins.guard
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
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
     "solidity"
     "graphql"]
    (r.join ",")))

(defn main []
  (when-let [guard (md.prequire :guard)]
    (let [ft (md.prequire :guard.filetype)]
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
        (ft :go)
        (: :fmt :lsp)
        (: :append :golines))
      (guard.setup {}))))
