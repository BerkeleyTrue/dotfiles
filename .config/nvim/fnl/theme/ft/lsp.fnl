(module theme.ft.lsp
  {require {: r
            : utils}})


(defn main [{: hi-link! : hi-clear}]
  (hi-link! :LspDiagnosticsUnderline :BerksNone)
  (hi-link! :LspDiagnosticsUnderlineHint :BerksNone)
  (hi-link! :LspDiagnosticsUnderlineInformation :BerksNone)

  (hi-link! :LspDiagnosticsInformation :BerksCyan)

           ; LspDiagnosticsUnderlineHint)
  (hi-link! :LspDiagnosticsHint :Comment)
  (hi-link! :LspDiagnosticsDefaultHint :Comment)
  (hi-link! :LspDiagnosticsFloatingHint :Comment)

  (hi-link! :LspDiagnosticsWarning :BerksOrange)
  (hi-link! :LspDiagnosticsUnderlineWarning :BerksWarnLine)

  (hi-link! :LspDiagnosticsError :BerksError)
  (hi-link! :LspDiagnosticsUnderlineError :BerksErrorLine))
