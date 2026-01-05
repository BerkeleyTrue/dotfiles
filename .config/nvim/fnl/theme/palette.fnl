(module theme.palette
  {autoload
   {a aniseed.core
    r r
    cl lib.color}
   require-macros [macros]})

; Based on Dracula
; deprecated, use named exports or hex field
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

(def hex
  {:rosewater "#f2d5cf"
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

(def rosewater [10 57 88])
(def flamingo  [0 59 84])
(def pink      [316 73 84])
(def mauve     [276 59 76])
(def red       [359 68 71])
(def maroon    [358 66 76])
(def peach     [20 79 70])
(def yellow    [40 62 73])
(def green     [96 44 68])
(def teal      [172 39 65])
(def sky       [189 48 73])
(def sapphire  [199 55 69])
(def blue      [222 74 74])
(def lavender  [239 66 84])
(def text      [227 70 87])
(def subtext1  [227 44 80])
(def subtext0  [228 29 73])
(def overlay2  [228 22 66])
(def overlay1  [227 17 58])
(def overlay0  [229 13 52])
(def surface2  [228 13 44])
(def surface1  [227 15 37])
(def surface0  [230 16 30])
(def base      [229 19 23])
(def mantle    [231 19 20])
(def crust     [229 20 17])

(def none      :NONE)


(defn get-color-by-name [name]
  (. palette name))

(comment (get-color-by-name :purple))
