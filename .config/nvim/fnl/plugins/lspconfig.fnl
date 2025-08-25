(module plugins.lspconfig
  {autoload
   {a aniseed.core
    navic nvim-navic
    schemastore schemastore
    r r}
   require {}
   import-macros []
   require-macros [macros]})


(defn open-hover-float []
  "disable cursor hold, open hover float, then re-enable cursor hold on cursor move"
  (command set "eventignore=CursorHold")
  (vim.lsp.buf.hover)
  (command autocmd "CursorMoved <buffer> ++once set eventignore=\"\""))

(defn on_attach [args]
  (let [client (assert (vim.lsp.get_client_by_id (. args :data :client_id)))]
    (when (client:supports_method :textDocument/documentSymbol)
      (navic.attach client (. args :buf)))

    ; remove dfeault reference keymap
    (vim.keymap.del :n :grr)
    (nnoremap :grd "<CMD>Telescope lsp_definitions<CR>" {:buffer buffnr :silent true})
    (nnoremap :grr "<CMD>Telescope lsp_references<CR>" {:buffer buffnr :silent true})
    (nnoremap :K open-hover-float {:buffer buffnr :silent true})

    ; always go through guard.nvim for formatting
    ; will need to add them to plugins.guard if you want to use them
    (nnoremap :zf "<CMD>Guard fmt<CR>" {:buffer buffnr :silent true})))

(defn tailwind-config []
  {:settings
   {:tailwindCSS
    {:emmetCompletions true
     :includeLanguages {:jinja :html}}}
   :filetypes (-> (. vim.lsp.config :tailwindcss :filetypes) 
                  (r.conj :jinja))})

(defn jsonls-configs []
  (let [schemas ((. schemastore :json :schemas))]
    {:settings
     {:json
      {: schemas}}}))

(defn open-float []
  "Check if cmp pum is visible, if not, show vim lsp diagnostics float")
  ; (when (not (and cmp (cmp.visible)))
  ;   (vim.diagnostic.open_float)))

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

(def lsps
  {:ansiblels {:settings {:ansible {:validation {:lint {:enable false}}}}}
   :bashls {}
   :clojure_lsp {}
   :cssls {}
   :dockerls {}
   :eslint {}
   :gopls {}
   :hls {}
   :html {}
   :jsonls (jsonls-configs)
   :lua_ls {}
   :nil_ls {}
   :nixd {}
   :basedpyright {}
   :prismals {}
   :purescriptls {}
   :rust_analyzer {}
   :slint_lsp {}
   :solidity_ls {}
   :templ {}
   :ts_ls {}
   :tailwindcss (tailwind-config)
   :vimls {}
   :yamlls {}})

(defn main []
  (set-configs)
  (augroup
    :MyLspAttach
    {:event [:LspAttach]
     :callback on_attach})
  (->> lsps
       (r.to-pairs)
       (r.for-each
         (fn [[lsp-name config]]
           (vim.lsp.config lsp-name config)
           (vim.lsp.enable lsp-name)))))
