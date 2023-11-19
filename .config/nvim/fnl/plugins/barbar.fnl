(module plugins.barbar
  {require
   {a aniseed.core
    r r
    md utils.module
    hl utils.highlights
    utils utils}
   require-macros [macros]})

(defn buff-delete-hidden []
  (when-let [bbye (md.prequire :barbar.bbye)]
    (let [buffers (->>
                    ; how many pages are there
                    (vf tabpagenr "$")
                    ; create an array of n (num of pages)
                    (r.range 1)
                    ; map page to buf list for that page
                    (r.map #(vf tabpagebuflist $1))
                    ; flatten
                    (r.reduce r.concat []))
          to-delete (->>
                      (vf bufnr "$")
                      ; creates a range of numbers up to the largest buffer number
                      (r.range 1)
                      ; buffer actually exists
                      (r.filter #(= (vf bufexists $1) 1))
                      ; buffer is not in the list above
                      (r.reject #(r.some (r.is-equal $1) buffers))
                      ; filter for normal buffers
                      (r.filter #(= (vf empty (vf getbufvar $1 "&buftype")) 1))
                      ; filter for unmodified buffers
                      (r.filter #(= (vf getbufvar $1 "&mod") 0)))]
      (when-not (r.empty? to-delete)
         (r.for-each #(bbye.bdelete $1) to-delete)
         (utils.print (.. "Closed " (r.size to-delete) " hidden buffers"))))))

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
       :inactive {:separator {:left "󱋱" :right " "}}}})
    (command! :Bdh buff-delete-hidden)))
