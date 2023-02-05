(module plugins.lspconfig
  {require
   {a aniseed.core
    r r
    utils utils
    md utils.module
    tsserver plugins.lspconfig.tsserver
    tailwindcss plugins.lspconfig.tailwindcss
    emmetls plugins.lspconfig.emmetls
    fennel-ls plugins.lspconfig.fennel-ls
    fennel-language-server plugins.lspconfig.fennel-language-server}
   require-macros [macros]})

(defn get-capabilities []
  (let [cmplsp (require :cmp_nvim_lsp)]
    (cmplsp.default_capabilities)))

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
      {:settings
       {:json
        {: schemas}}})
    {}))


(defn- general-on-attach [client buffnr]
  (nnoremap :zf "<CMD>lua vim.lsp.buf.format({ async = true })<CR>" {:buffer buffnr :silent true})
  (nnoremap :K "<CMD>lua vim.lsp.buf.hover()<CR>" {:buffer buffnr :silent true})
  (nnoremap :gd "<CMD>lua vim.lsp.buf.definition()<CR>" {:buffer buffnr :silent true})
  (nnoremap :zca "<CMD>lua vim.lsp.buf.code_action()<CR>" {:buffer buffnr :silent true})
  (nnoremap :zrn "<CMD>lua vim.lsp.buf.rename()<CR>" {:buffer buffnr :silent true}))

(def general-on-attach-with-navic
  (r.over
    general-on-attach
    (fn [client buffnr]
      (when client.server_capabilities.documentSymbolProvider
        (when-let [navic (md.prequire :nvim-navic)]
          (navic.attach client buffnr))))))

(defn ansible-configs []
  {:on_attach general-on-attach-with-navic
   :settings
   {:ansible
    {:validation
     {:lint
      {:enable false}}}}})

(def lsps
  {:ansiblels (ansible-configs)
   :bashls {:on_attach general-on-attach-with-navic}
   :cssls {}
   :dockerls {}
   :emmet_ls (emmetls.get-config)
   :eslint {}
   ; :fennel_ls {} ; disabled for now, since it doesn't pick up macros
   ; :fennel_language_server {} ; disabled for now, since it doesn't pick up macros
   :gopls {:on_attach general-on-attach-with-navic}
   :hls {:on_attach general-on-attach-with-navic}
   :html {}
   :jsonls (jsonls-configs)
   :nil_ls {}
   :prismals {}
   :solidity_ls {}
   :sumneko_lua {}
   :tsserver (tsserver.get-config {:on_attach general-on-attach-with-navic})
   :tailwindcss (tailwindcss.get-config)
   :vimls {:on_attach general-on-attach-with-navic}
   :yamlls {}})


(defn open-float []
  "Check if cmp pum is visible, if not, show vim lsp diagnostics float"
  (let [cmp (md.prequire :cmp)]
    (when (not (and cmp (cmp.visible)))
      (vim.diagnostic.open_float))))

(defn- set-configs []
  (vim.diagnostic.config
    {:virtual_text true
     :signs true
     ; Configure float appearence
     :float
     {:border :rounded
      :focus false
      :max_width 60
      :scope "cursor"
      :header false}})

  ; open vim diagnotics float on cursor hold
  (augroup
    :DiagnositicFloat
    {:event [:CursorHold :CursorHoldI]
     :pattern :*
     :callback open-float})

  (command! :Format ":lua vim.lsp.buf.formatting()")
  (command! :LspDiagnostics "lua vim.diagnostic.setloclist()")
  (noremap  :zj vim.diagnostic.goto_next {:silent true})
  (noremap  :zk vim.diagnostic.goto_prev {:silent true}))

(defn main []
  (set-configs)
  (when-let [lspconfig (md.prequire :lspconfig)]
    (let [configs (require :lspconfig.configs)
          lsputil (require :lspconfig.util)]

      (when-not configs.caramel_lsp
        (set configs.caramel_lsp (caramel-configs lsputil)))

      (when-not configs.fennel_ls
        (set configs.fennel_ls (fennel_ls.main lsputil)))

      (when-not configs.fennel_language_server
        (set configs.fennel_language_server (fennel-language-server.main lsputil)))

      (when (= (os.execute "which sourcekit-lsp > /dev/null 2>&1") 0)
        (set lsps.sourcekit {}))

      (->>
        lsps
        (r.to-pairs)
        (r.for-each
          (fn [[lsp-name config]]
            (let [lsp-module (. lspconfig lsp-name)
                  lsp-setup (. lsp-module :setup)]
              (lsp-setup
                (r.merge
                  config
                  {:capabilities (get-capabilities)
                   :on_attach (or (. config :on_attach) general-on-attach)})))))))))
