(module palette
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

; Based on Dracula
(def palette
  {:fg        ["#F8F8F2" 253]
   :bg        ["#282A36" 236]
   :bglighter ["#424450" 238]
   :bglight   ["#343746" 237]
   :bgdark    ["#21222C" 235]
   :bgdarker  ["#191A21" 234]

   :comment   ["#6272A4"  61]
   :selection ["#44475A" 239]
   :subtle    ["#424450" 238]
   :gray      ["#808080"]

   :cyan      ["#8BE9FD" 117]
   :green     ["#50FA7B"  84]
   :orange    ["#FFB86C" 215]
   :pink      ["#FF79C6" 212]
   :purple    ["#BD93F9" 141]
   :red       ["#FF5555" 203]
   :yellow    ["#F1FA8C" 228]})
