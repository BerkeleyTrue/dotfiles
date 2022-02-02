(module plugins.null-ls
  {:require
   {utils utils
    md utils.module
    a aniseed.core
    zprint plugins.null-ls.zprint
    yamlfix plugins.null-ls.yamlfix}
   :require-macros [macros]})

(defn main []
  (let [null-ls (md.packadd-n-require :null-ls.nvim :null-ls)
        sources [(null-ls.builtins.formatting.prettier.with
                   {:command "npx"
                    :args ["prettier" "--stdin-filepath" "$FILENAME"]})
                 (zprint.main null-ls)
                 (null-ls.builtins.formatting.shfmt.with
                   {:extra_args [:-i 2]})
                 (yamlfix.main null-ls)]]


    (null-ls.setup {: sources})))
