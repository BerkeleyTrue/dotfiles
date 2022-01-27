; TODO: move into plugins.barbar once theme is idempodent
(module theme.plugins.ale
  {require {: utils}
   require-macros [macros]})

(defn main [{: hi-link! : c : add-group}]
  (hi-link! :ALEVirtualTextError :Error)
  (hi-link! :ALEVirtualTextWarning :Todo))
