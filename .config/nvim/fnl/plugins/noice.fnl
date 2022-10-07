(module plugins.noice
  {require
   {a aniseed.core
    anenv plugins.aniseed
    md utils.module
    packer plugins.packer
    utils utils}
   require-macros [macros]})

(def- config
  {:cmdline
   {:view :cmdline_popup
    :opts {:buf_options {:filetype :vim}}
    :icons
    {:/ {:icon " " :hl_group :DiagnosticWarn}
     :? {:icon " " :hl_group :DiagnosticWarn}
     ":" {:icon " "
          :hl_group :DiagnosticInfo
          :firstc false}}}
   :notify {:enabled false}
   :history {:enabled false}
   :messages {:enabled false}
   :throttle (/ 1000 30)
   :views {}
   :routes {}})

(defn main []
  (when-let [noice (md.prequire :noice)]
    (noice.setup config)))
