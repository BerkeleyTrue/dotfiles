(module plugins.beacon
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    beacon beacon}
   require-macros [macros]})

; beacon doesn't work with auto-session yet
(defn main [])
  ; (beacon.setup {}))
