(module plugins.lspconfig
  {:require {: r
             : utils
             null-ls plugins.null-ls}

   :require-macros [macros]})

(def lsps
  {:bashls {}
   :caramel_lsp {}
   :clojure_lsp {}
   :cssls {}
   :dockerls {}
   :hls {}
   :html {}
   :jsonls {}
   :null-ls {:on_attach
             (fn [client]
               (when client.resolved_capabilities.document_formatting
                 (vim.cmd "autocmd BufWritePost <buffer> lua vim.lsp.buf.formatting()")))}

   :rls {}
   :tsserver {:on_attach
              (fn [client]
                (tset client.resolved_capabilities :document_formatting false))}
   :vimls {}
   :yamlls {}})


(defn main []
  (let [(ok res) (pcall utils.ex.packadd :nvim-lspconfig)]
    (null-ls.main)

    (if (not ok) (print (.. "Could not load nvim-lspconfig: " (tostring res)))
      (let [(ok lspconfig) (pcall require :lspconfig)]
        (if (not ok) (print (.. "require: " lspconfig))
          (let [lspcnf (require :lspconfig/configs)
                lsputil (require :lspconfig/util)]

            (when (not lspcnf.caramel_lsp)
              (set
                lspcnf.caramel_lsp
                {:default_config
                 {:cmd [:caramel-lsp :start]
                  :filetypes [:ocaml]
                  :root_dir (lsputil.root_pattern ".merlin" "package.json" ".git")
                  :settings {}}}))
            (->>
              lsps
              (r.to-pairs)
              (r.for-each
                (fn [[lsp config]]
                  (let [conf (. lspconfig lsp)
                        setup (. conf :setup)]
                    (setup config)))))))))))
