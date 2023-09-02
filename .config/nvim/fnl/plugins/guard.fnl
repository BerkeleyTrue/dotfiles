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
      (doto (ft prettier-fts)
        (: :fmt :prettier))
      (doto (ft :nix)
        (: :fmt {:cmd "nix" :args ["fmt" "--" "-c" "--quiet"] :stdin true})) ; requires alejandra for formatting
      (guard.setup {}))))
