(module theme.palette
  {require
   {r r}
   require-macros [macros]})

; Based on Dracula
; Moving to Catpuccin Frappe
(def palette
  {:fg        "#F8F8F2"
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




(defn get-color-by-name [name]
  (. palette name))

(comment (get-color-by-name :purple))
