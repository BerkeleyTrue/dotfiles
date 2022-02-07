(module plugins.lspconfig
  {require
   {a aniseed.core
    r r
    utils utils
    md utils.module
    tsserver plugins.lspconfig.tsserver}

   require-macros [macros]})

(defn get-capabilities []
  (let [cmplsp (require :cmp_nvim_lsp)]
    (cmplsp.update_capabilities (vim.lsp.protocol.make_client_capabilities))))

(defn caramel-configs [lsputil]
  {:default_config
   {:cmd [:caramel-lsp :start]
    :filetypes [:ocaml]
    :root_dir (lsputil.root_pattern ".merlin" "package.json" ".git")
    :settings {}}})

(defn jsonls-configs []
  (if-let [schemastore (md.prequire :schemastore)]
    (let [schemas ((. schemastore :json :schemas))
          base-conf (md.prequire :lspconfig.server_configurations.jsonls)]
      (r.merge base-conf {:settings {:json {: schemas}}}))
    {}))

(def lsps
  {:ansiblels {}
   :bashls {}
   :caramel_lsp {}
   :clojure_lsp
   {:on_attach
    (fn [client]
      ; rely on zprint
      (tset client.resolved_capabilities :document_formatting false))}
   :cssls {}
   :dockerls {}
   :hls {}
   :html {}
   :jsonls {}
   :rls {}
   :tsserver (tsserver.get-config)
   :vimls {}
   :yamlls {}})

(defn- set-configs []
  (vim.diagnostic.config
    {:virtual_text true
     :signs true
     :float
     {:border :rounded
      :focus false
      :max_width 60
      :scope "cursor"
      :header false}})

  (utils.augroup
    :diagnositic-float
    [{:event [:CursorHold :CursorHoldI]
      :pattern :*
      :cmd "lua vim.diagnostic.open_float()"}])

  (utils.nnoremap :zf ":lua vim.lsp.buf.formatting()<CR>")
  (utils.ex.command_ :Format ":lua vim.lsp.buf.formatting()"))

(defn main []
  (set-configs)
  (when-let [lspconfig (md.packadd-n-require :nvim-lspconfig :lspconfig)]
    (let [configs (require :lspconfig.configs)
          lsputil (require :lspconfig.util)]

      (when-not configs.caramel_lsp
        (set configs.caramel_lsp (caramel-configs lsputil)))
      (set configs.jsonls (jsonls-configs))

      (->>
        lsps
        (r.to-pairs)
        (r.for-each
          (fn [[lsp-name config]]
            (let [lsp-module (. lspconfig lsp-name)
                  lsp-setup (. lsp-module :setup)]
              (lsp-setup (r.merge config {:capabilities (get-capabilities)})))))))))
