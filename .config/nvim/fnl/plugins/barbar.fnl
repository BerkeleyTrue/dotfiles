(module plugins.barbar
  {require
   {a aniseed.core
    r r
    md utils.module
    hl utils.highlights
    utils utils}
   require-macros [macros]})

(defn init []
  (hl.link! :BufferCurrent :Normal)
  (hl.link! :BufferVisible :BerksComment)
  (hl.link! :BufferInactive :BerksGray)

  (hl.link! :BufferCurrentMod :BerksOrangeBold)
  (hl.link! :BufferVisibleMod  :BerksOrange)
  (hl.link! :BufferInactiveMod :BerksOrangeLight)

  ; divider
  (hl.link! :BufferCurrentSign :BerksCyan)
  (hl.link! :BufferVisibleSign :BerksPinkDark)
  (hl.link! :BufferInactiveSign :BerksPurple))

(defn main []
  (when-let [barbar (md.prequire :barbar)]
    (barbar.setup
     {:icons
      {:button " "
       :modified {:button " "}
       :separator {:left "󱋱" :right " "}
       :separator_at_end false
       :inactive {:separator {:left "󱋱" :right " "}}}})))
