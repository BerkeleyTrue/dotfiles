(module plugins.barbar
  {require
   {a aniseed.core
    r r
    md utils.module
    hl utils.highlights
    utils utils}
   require-macros [macros]})


(defn main []
  (hl.link! :BufferCurrent :Normal)
  (hl.link! :BufferCurrentMod :BerksRed)
  (hl.link! :BufferCurrentSign :BerksPink)
  (hl.link! :BufferVisible :BerksComment)
  (hl.link! :BufferVisibleMod :BerksDiffDelete)
  (hl.link! :BufferVisibleSign :BerksPinkDark))
  ; (hi-link! :BufferCurrentTarget)
  ; (hi-link! :BufferVisibleTarget))

  ; (hi-link! :BufferInactive)
  ; (hi-link! :BufferInactiveMod)
  ; (hi-link! :BufferInactiveSign)
  ; (hi-link! :BufferInactiveTarget))

  ; (hi-link! :BufferTabpages)
  ; (hi-link! :BufferTabpageFill))
