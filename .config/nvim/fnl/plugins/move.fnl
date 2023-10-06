(module plugins.move
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(def- moveKeyModifier :A)

(defn- create-move-key [key]
  (.. "<" moveKeyModifier "-" key ">"))

(defn init []
  (vnoremap (create-move-key :j) ":MoveBlock(1)<CR>" {:silent true})
  (vnoremap (create-move-key :k) ":MoveBlock(-1)<CR>" {:silent true})

  ; (vnoremap (create-move-key :h) ":MoveHBlock(-1)<CR>" {:silent true}) // don't work right?
  ; (vnoremap (create-move-key :l) ":MoveHBlock(1)<CR>" {:silent true})

  (nnoremap (create-move-key :j) ":MoveLine(1)<CR>" {:silent true})
  (nnoremap (create-move-key :k) ":MoveLine(-1)<CR>" {:silent true})

  (nnoremap (create-move-key :h) ":MoveHChar(-1)<CR>" {:silent true})
  (nnoremap (create-move-key :l) ":MoveHChar(1)<CR>" {:silent true}))
