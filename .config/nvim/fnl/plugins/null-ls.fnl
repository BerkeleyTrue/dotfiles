(module plugins.null-ls
  {require
   {utils utils
    md utils.module
    a aniseed.core
    zprint plugins.null-ls.zprint
    yamlfix plugins.null-ls.yamlfix}
   require-macros [macros]})

(defn main []
  (let [null-ls (md.prequire :null-ls)
        sources [(null-ls.builtins.formatting.prettier.with
                   {:command "npx"
                    :args ["prettier" "--stdin-filepath" "$FILENAME"]

                    :filetypes
                    [:javascript
                     :javascriptreact
                     :typescript
                     :typescriptreact
                     :vue
                     :css
                     :scss
                     :less
                     :html
                     :json
                     :jsonc
                     :yaml
                     :markdown
                     :solidity
                     :graphql]})
                 (zprint.main null-ls)
                 (null-ls.builtins.formatting.shfmt.with
                   {:extra_args [:-i 2]})
                 (yamlfix.main null-ls)
                 null-ls.builtins.formatting.lua_format]]


    (null-ls.setup {: sources})))
