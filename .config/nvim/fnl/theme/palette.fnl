(module theme.palette
  {autoload
   {a aniseed.core
    r r}
   require-macros [macros]})

; Based on Dracula
; Moving to Catpuccin Frappe
(def palette
  {
   :fg        "#F8F8F2"
   :bg        "#282A36"
   :bglighter "#424450"
   :bglight   "#343746"
   :bgdark    "#21222C"
   :bgdarker  "#191A21"

   :comment   "#6272A4"
   :selection "#44475A"
   :subtle    "#424450"
   :gray      "#808080"

   :cyan      "#8BE9FD"
   :orange    "#FFB86C"
   :purple    "#BD93F9"

   ; Catpuccino Frappe
   :rosewater "#f2d5cf"
   :flamingo  "#eebebe"
   :pink      "#f4b8e4"
   :mauve     "#ca9ee6"
   :red       "#e78284"
   :maroon    "#ea999c"
   :peach     "#ef9f76"
   :yellow    "#e5c890"
   :green     "#a6d189"
   :teal      "#81c8be"
   :sky       "#99d1db"
   :sapphire  "#85c1dc"
   :blue      "#8caaee"
   :lavender  "#babbf1"
   :text      "#c6d0f5"
   :subtext1  "#b5bfe2"
   :subtext0  "#a5adce"
   :overlay2  "#949cbb"
   :overlay1  "#838ba7"
   :overlay0  "#737994"
   :surface2  "#626880"
   :surface1  "#51576d"
   :surface0  "#414559"
   :base      "#303446"
   :mantle    "#292c3c"
   :crust     "#232634"})

(def rosewater "#f2d5cf")
(def flamingo  "#eebebe")
(def pink      "#f4b8e4")
(def mauve     "#ca9ee6")
(def red       "#e78284")
(def maroon    "#ea999c")
(def peach     "#ef9f76")
(def yellow    "#e5c890")
(def green     "#a6d189")
(def teal      "#81c8be")
(def sky       "#99d1db")
(def sapphire  "#85c1dc")
(def blue      "#8caaee")
(def lavender  "#babbf1")
(def text      "#c6d0f5")
(def subtext1  "#b5bfe2")
(def subtext0  "#a5adce")
(def overlay2  "#949cbb")
(def overlay1  "#838ba7")
(def overlay0  "#737994")
(def surface2  "#626880")
(def surface1  "#51576d")
(def surface0  "#414559")
(def base      "#303446")
(def mantle    "#292c3c")
(def crust     "#232634")


(defn get-color-by-name [name]
  (. palette name))

(comment (get-color-by-name :purple))
