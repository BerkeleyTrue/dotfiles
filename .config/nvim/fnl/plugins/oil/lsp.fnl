(module plugins.oil.lsp
  {autoload
   {a aniseed.core
    r r
    lsp oil-lsp-diagnostics}
   require {}
   import-macros []
   require-macros [macros]})


(defn main []
  (lsp.setup {:diagnostics_color {:error :BerksError
                                  :warn :BerksWarn
                                  :info :DiagnosticInfo
                                  :hint :DiagnosticHint}
              :diagnostic_symbols {:error "  "
                                   :warn "  "
                                   :info "  "
                                   :hint "  "}}))
