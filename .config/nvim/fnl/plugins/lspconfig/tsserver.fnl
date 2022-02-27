(module plugins.lspconfig.tsserver
  {require
   {a aniseed.core
    r r
    utils utils
    md utils.module}
   require-macros [macros]})

(defn get-config [conf]
  (let [{: on_attach} (or conf {})
        ts-utils (md.packadd-n-require :nvim-lsp-ts-utils)]
    {:init_options (when ts-utils (. ts-utils :init_options))
     :on_attach
     (fn [client bufnr]
       ; rely on prettier
       (tset client.resolved_capabilities :document_formatting false)
       (when ts-utils
         (ts-utils.setup {})
         (ts-utils.setup_client client))
       (when on_attach
         (on_attach client bufnr)))}))
