(module theme.ft.lsp
  {require
   {: r
    : utils
    hl utils.highlights}})


(defn main []
  (hl.link! :LspDiagnosticsUnderline :BerksNone)
  (hl.link! :LspDiagnosticsUnderlineHint :BerksNone)
  (hl.link! :LspDiagnosticsUnderlineInformation :BerksNone)

  (hl.link! :LspDiagnosticsInformation :BerksCyan)

           ; LspDiagnosticsUnderlineHint)
  (hl.link! :LspDiagnosticsHint :Comment)
  (hl.link! :LspDiagnosticsDefaultHint :Comment)
  (hl.link! :LspDiagnosticsFloatingHint :Comment)

  (hl.link! :LspDiagnosticsWarning :BerksOrange)
  (hl.link! :LspDiagnosticsUnderlineWarning :BerksWarnLine)

  (hl.link! :LspDiagnosticsError :BerksError)
  (hl.link! :LspDiagnosticsUnderlineError :BerksErrorLine))
