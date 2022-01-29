(module plugins.lspconfig.tsserver
  {:require
   {a aniseed.core
    r r
    utils utils
    md utils.module}
   :require-macros [macros]})

(defn get-config []
  (let [ts-utils (md.packadd-n-require :nvim-lsp-ts-utils)]
    {:init_options (when ts-utils (. ts-utils :init_options))
     :on_attach
     (fn [client]
       ; rely on prettier
       (tset client.resolved_capabilities :document_formatting false)
       (when ts-utils
         (ts-utils.setup {})
         (ts-utils.setup_client client)))}))
