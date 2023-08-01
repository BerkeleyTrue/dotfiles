(module plugins.colorbuddy
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn- create-add-group [{: c : s : G}]
  (fn add-group [name fg bg? style?]
    "Add a new group to colorbuddy"
    (let [bg (or bg? c.none)
          style (or style? s.none)]
      (G.new name fg bg style))))

(defn main [palette]
  (when-let [cb (md.prequire :colorbuddy)]
    ; add palette groups to colorbuddy
    (->>
      palette
      (r.to-pairs)
      (r.for-each
        (fn [[name colors]]
          (cb.Color.new name (a.first colors)))))

    {:c  cb.colors
     :s cb.styles
     :add-group
     (create-add-group
       {:c cb.colors
        :s cb.styles
        :G cb.Group})}))
