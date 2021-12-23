(module plugins.null-ls
  {:require {: utils}
   :require-macros [macros]})

(defn main []
  (let [(ok res) (pcall utils.ex.packadd :null-ls.nvim)]
    (if (not ok) (print (.. "Could not load null-ls: " (tostring res)))
      (let [(ok null-ls) (pcall require :null-ls)]
        (if (not ok) (print (.. "require null-ls: ") null-ls)
          (let [sources [(null-ls.builtins.formatting.prettier.with
                           {:command "npx"
                            :args ["prettier" "--stdin-filepath" "$FILENAME"]})]]

            (null-ls.setup {: sources})))))))
