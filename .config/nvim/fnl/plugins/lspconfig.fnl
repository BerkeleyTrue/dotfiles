(module plugins.lspconfig
  {require
   {a aniseed.core
    r r
    utils utils
    md utils.module
    tsserver plugins.lspconfig.tsserver
    tailwindcss plugins.lspconfig.tailwindcss
    emmetls plugins.lspconfig.emmetls
    fennel-language-server plugins.lspconfig.fennel-language-server
    fennel_ls plugins.lspconfig.fennel-ls}
   require-macros [macros]})

(defn get-capabilities []
  (when-let [cmplsp (md.prequire :cmp_nvim_lsp)]
    (cmplsp.default_capabilities)))

(defn create-disable-formatting [on-attach]
  (fn [client buffnr]
    (tset client.server_capabilities :documentFormattingProvider false)
    (on-attach client buffnr)))

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

(defn open-hover-float []
  "disable cursor hold, open hover float, then re-enable cursor hold on cursor move"
  (command set "eventignore=CursorHold")
  (vim.lsp.buf.hover)
  (command autocmd "CursorMoved <buffer> ++once set eventignore=\"\""))


(defn- general-on-attach [client buffnr]
  (nnoremap :gd "<CMD>Telescope lsp_definitions<CR>" {:buffer buffnr :silent true})
  (nnoremap :gr "<CMD>Telescope lsp_references<CR>" {:buffer buffnr :silent true})
  (nnoremap :zca "<CMD>lua vim.lsp.buf.code_action()<CR>" {:buffer buffnr :silent true})
  (nnoremap :zrn "<CMD>lua vim.lsp.buf.rename()<CR>" {:buffer buffnr :silent true})
  (nnoremap :K open-hover-float {:buffer buffnr :silent true})

  ; always go through guard.nvim for formatting
  ; will need to add them to plugins.guard if you want to use them
  (nnoremap :zf "<CMD>GuardFmt<CR>" {:buffer buffnr :silent true}))

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
   :clojure_lsp {:on_attach general-on-attach-with-navic}
   :cssls {}
   :dockerls {}
   :emmet_ls (emmetls.get-config)
   :eslint {}
   :gopls {:on_attach general-on-attach-with-navic}
   :hls {:on_attach general-on-attach-with-navic}
   :html {:on_attach (create-disable-formatting general-on-attach)}
   :jsonls (jsonls-configs)
   :lua_ls {}
   :nil_ls {:on_attach (create-disable-formatting general-on-attach-with-navic)}
   :nixd {:on_attach (create-disable-formatting general-on-attach)}
   :prismals {}
   :purescriptls {}
   :rust_analyzer {}
   :slint_lsp {}
   :solidity_ls {}
   :templ {:on_attach general-on-attach-with-navic}
   :ts_ls (tsserver.get-config {:on_attach general-on-attach-with-navic})
   :tailwindcss (tailwindcss.get-config)
   :vimls {:on_attach general-on-attach-with-navic}
   :yamlls {}})


(defn open-float []
  "Check if cmp pum is visible, if not, show vim lsp diagnostics float"
  (let [cmp (md.prequire :cmp)]
    (when (not (and cmp (cmp.visible)))
      (vim.diagnostic.open_float))))

(defn go-to-next []
  "get next error, if no errors, get next warning, if no warnings, get next info"
  (let [errors (vim.diagnostic.get 0 {:severity vim.diagnostic.severity.ERROR})]
    (if (not (r.empty? errors))
      (vim.diagnostic.goto_next {:severity vim.diagnostic.severity.ERROR})
      (let [warnings (vim.diagnostic.get 0 {:severity vim.diagnostic.severity.WARNING})]
        (if (not (r.empty? warnings))
          (vim.diagnostic.goto_next {:severity vim.diagnostic.severity.WARNING})
          (vim.diagnostic.goto_next {:severity vim.diagnostic.severity.INFO}))))))

(defn go-to-prev []
  "get prev error, if no errors, get prev warning, if no warnings, get prev info"
  (let [errors (vim.diagnostic.get 0 {:severity vim.diagnostic.severity.ERROR})]
    (if (not (r.empty? errors))
      (vim.diagnostic.goto_prev {:severity vim.diagnostic.severity.ERROR})
      (let [warnings (vim.diagnostic.get 0 {:severity vim.diagnostic.severity.WARNING})]
        (if (not (r.empty? warnings))
          (vim.diagnostic.goto_prev {:severity vim.diagnostic.severity.WARNING})
          (vim.diagnostic.goto_prev {:severity vim.diagnostic.severity.INFO}))))))

(defn- set-configs []
  (vim.diagnostic.config
    {:virtual_text true
     :signs true
     ; Configure float appearence
     :float
     {:border :rounded
      :focus false
      :max_width 120
      :scope "cursor"
      :title "Diagnostics"
      :header false}})


  ; open vim diagnotics float on cursor hold
  (augroup
    :DiagnositicFloat
    {:event [:CursorHold :CursorHoldI]
     :pattern :*
     :callback open-float})

  (command! :Format ":lua vim.lsp.buf.formatting()")
  (command! :LspDiagnostics "lua vim.diagnostic.setloclist()")
  (noremap  :zk go-to-prev {:silent true})
  (noremap  :zj go-to-next {:silent true}))

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
