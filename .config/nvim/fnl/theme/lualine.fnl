(module theme.lualine
  {autoload
   {a aniseed.core
    r r
    md utils.module
    p theme.palette
    utils utils
    cl lib.color}
   require-macros [macros]})

(def- hx p.hex)

(def theme
 {:normal
  {:a {:fg hx.base :bg hx.blue :gui :bold}
   :b {:fg hx.blue :bg hx.surface1}
   :c {:fg hx.text :bg hx.base}}
  :insert
  {:a {:bg hx.green :fg hx.base :gui :bold}
   :b {:bg hx.surface1 :fg hx.teal}} :terminal
  {:a {:bg hx.green :fg hx.base :gui :bold}
   :b {:bg hx.surface1 :fg hx.teal}}
  :command
  {:a {:bg hx.peach :fg hx.base :gui :bold}
   :b {:bg hx.surface1 :fg hx.peach}}
  :visual
  {:a {:bg hx.mauve :fg hx.base :gui :bold}
   :b {:bg hx.surface1 :fg hx.mauve}}
  :replace
  {:a {:bg hx.red :fg hx.base :gui :bold}
   :b {:bg hx.surface1 :fg hx.red}}
  :inactive
  {:a {:bg hx.base :fg hx.blue}
   :b {:bg hx.base :fg hx.surface1 :gui :bold}
   :c {:bg hx.base :fg hx.overlay0}}})
