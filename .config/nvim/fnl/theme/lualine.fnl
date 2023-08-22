(module theme.lualine
  {require
   {a aniseed.core
    r r
    md utils.module
    p theme.palette
    utils utils}
   require-macros [macros]})

(def- C (. p :palette))

(def theme
 {:normal
  {:a {:bg C.blue :fg C.mantle :gui :bold}
   :b {:bg C.surface1 :fg C.blue}
   :c {:bg C.mantle :fg C.text}}
  :insert
  {:a {:bg C.green :fg C.base :gui :bold}
   :b {:bg C.surface1 :fg C.teal}}
  :terminal
  {:a {:bg C.green :fg C.base :gui :bold}
   :b {:bg C.surface1 :fg C.teal}}
  :command
  {:a {:bg C.peach :fg C.base :gui :bold}
   :b {:bg C.surface1 :fg C.peach}}
  :visual
  {:a {:bg C.mauve :fg C.base :gui :bold}
   :b {:bg C.surface1 :fg C.mauve}}
  :replace
  {:a {:bg C.red :fg C.base :gui :bold}
   :b {:bg C.surface1 :fg C.red}}
  :inactive
  {:a {:bg C.mantle :fg C.blue}
   :b {:bg C.mantle :fg C.surface1 :gui :bold}
   :c {:bg C.mantle :fg C.overlay0}}})
