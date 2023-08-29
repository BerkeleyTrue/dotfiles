(module theme.lualine
  {require
   {a aniseed.core
    r r
    md utils.module
    p theme.palette
    utils utils}
   require-macros [macros]})

(def- c (. p :palette))

(def theme
 {:normal
  {:a {:bg c.blue :fg c.base :gui :bold}
   :b {:bg c.surface1 :fg c.blue}
   :c {:bg c.base :fg c.text}}
  :insert
  {:a {:bg c.green :fg c.base :gui :bold}
   :b {:bg c.surface1 :fg c.teal}}
  :terminal
  {:a {:bg c.green :fg c.base :gui :bold}
   :b {:bg c.surface1 :fg c.teal}}
  :command
  {:a {:bg c.peach :fg c.base :gui :bold}
   :b {:bg c.surface1 :fg c.peach}}
  :visual
  {:a {:bg c.mauve :fg c.base :gui :bold}
   :b {:bg c.surface1 :fg c.mauve}}
  :replace
  {:a {:bg c.red :fg c.base :gui :bold}
   :b {:bg c.surface1 :fg c.red}}
  :inactive
  {:a {:bg c.base :fg c.blue}
   :b {:bg c.base :fg c.surface1 :gui :bold}
   :c {:bg c.base :fg c.overlay0}}})
