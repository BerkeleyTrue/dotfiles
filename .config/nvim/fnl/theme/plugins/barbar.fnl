; TODO: move into plugins.barbar once theme is idempodent
(module theme.plugins.barbar
  {require {: utils}
   require-macros [macros]})

(defn main [{: hi-link! : c : add-group}]
  (hi-link! :BufferCurrent :Normal)
  (hi-link! :BufferCurrentMod :BerksRed)
  (hi-link! :BufferCurrentSign :BerksPink)
  ; (hi-link! :BufferCurrentTarget)
  (hi-link! :BufferVisible :BerksComment)
  (hi-link! :BufferVisibleMod :BerksDiffDelete)
  (add-group :BufferVisibleSign c.pink c.bgdark))
  ; (hi-link! :BufferVisibleTarget))

  ; (hi-link! :BufferInactive)
  ; (hi-link! :BufferInactiveMod)
  ; (hi-link! :BufferInactiveSign)
  ; (hi-link! :BufferInactiveTarget))

  ; (hi-link! :BufferTabpages)
  ; (hi-link! :BufferTabpageFill))
