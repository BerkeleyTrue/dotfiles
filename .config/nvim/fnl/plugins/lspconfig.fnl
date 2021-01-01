(module plugins.lspconfig
  {:require {: r
             : utils}
   :require-macros [macros]})

(def lsps
  {:tsserver {}
   :vimls {}
   :bashls {}
   :cssls {}
   :dockerls {}
   :html {}
   :jsonls {}
   :rls {}
   ; :rust_analyzer {}
   ; :texlab {}
   :yamlls {}})


(defn main []
  (let [(ok res) (pcall utils.ex.packadd :nvim-lspconfig)]
    (if (not ok) (print (.. "Could not load nvim-lspconfig: " (tostring res)))
      (let [lspconfig (require :lspconfig)]
        (->>
          lsps
          (r.to-pairs)
          (r.for-each (fn [[lsp config]] ((. (. lspconfig lsp) :setup) config))))))))
