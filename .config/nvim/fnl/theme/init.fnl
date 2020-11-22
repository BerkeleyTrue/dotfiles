(module theme
  {:require {a aniseed.core
             nvim aniseed.nvim
             str aniseed.string
             cb colorbuddy
             log colorbuddy.log
             utils utils}})



(comment (set log.level "debug"))

(def- c cb.colors)
(def- C cb.Color)
(def- g cb.groups)
(def- G cb.Group)
(def- s cb.styles)
(def- hi-link! utils.hi-link!)

(defn add-group [name fg bg? style?]
  "Add a new group to colorbuddy"
  (let [bg (or bg? c.none)
        style (or style? s.none)]
    (G.new name fg bg style)))

; Based on Dracula
(def- palette {:fg        ["#F8F8F2" 253]
               :bg        ["#282A36" 236]
               :bglighter ["#424450" 238]
               :bglight   ["#343746" 237]
               :bgdark    ["#21222C" 235]
               :bgdarker  ["#191A21" 234]

               :comment   ["#6272A4"  61]
               :selection ["#44475A" 239]
               :subtle    ["#424450" 238]

               :cyan      ["#8BE9FD" 117]
               :green     ["#50FA7B"  84]
               :orange    ["#FFB86C" 215]
               :pink      ["#FF79C6" 212]
               :purple    ["#BD93F9" 141]
               :red       ["#FF5555" 203]
               :yellow    ["#F1FA8C" 228]})

; Base Highlights colors
(each [name colors (pairs palette)]
  (C.new name (a.first colors)))

(add-group :BerksBg c.none c.bg)
(add-group :BerksBgLight c.none c.bglight)
(add-group :BerksBgLighter c.none c.bglighter)
(add-group :BerksBgDark c.none c.bgdark)
(add-group :BerksBgDarker c.none c.bgdarker)

(add-group :BerksFg c.fg)
(add-group :BerksFgUnderline c.fg c.none s.underline)
(add-group :BerksFgBold c.fg c.none s.bold)

(add-group :BerksComment c.comment)
(add-group :BerksCommentBold c.comment c.none s.bold)

(add-group :BerksSelection c.none c.selection)

(add-group :BerksSubtle c.subtle)

(add-group :BerksCyan c.cyan)
(add-group :BerksCyanItalic c.cyan c.none s.italic)

(add-group :BerksGreen c.green)
(add-group :BerksGreenBold c.green c.none s.bold)
(add-group :BerksGreenItalic c.green c.none s.italic)
(add-group :BerksGreenItalicUnderline c.green c.none (+ s.italic s.underline))

(add-group :BerksOrange c.orange)
(add-group :BerksOrangeBold c.orange c.none s.bold)
(add-group :BerksOrangeItalic c.orange c.none s.italic)
(add-group :BerksOrangeBoldItalic c.orange c.none (+ s.bold s.italic))
(add-group :BerksOrangeInverse c.bg c.orange)

(add-group :BerksPink c.pink)
(add-group :BerksPinkItalic c.pink c.none s.italic)

(add-group :BerksPurple c.purple)
(add-group :BerksPurpleBold c.purple c.none s.bold)
(add-group :BerksPurpleItalic c.purple c.none s.italic)

(add-group :BerksRed c.red)
(add-group :BerksRedInverse c.fg c.red)

(add-group :BerksYellow c.yellow)
(add-group :BerksYellowItalic c.yellow c.none s.italic)

(add-group :BerksError c.red c.none)
(add-group :BerksWarn c.orange c.none)

(add-group :BerksErrorLine c.red c.none s.undercurl)
(add-group :BerksWarnLine c.orange c.none s.undercurl)
(add-group :BerksInfoLine c.cyan c.none s.undercurl)
(add-group :BerksTodo c.cyan c.none (+ s.bold s.inverse))
(add-group :BerksSearch c.green c.none s.inverse)
(add-group :BerksBoundary c.comment c.bgdark)
(add-group :BerksLink c.cyan c.none s.underline)

(add-group :BerksDiffChange c.orange c.none)
(add-group :BerksDiffText c.bg c.orange)
(add-group :BerksDiffDelete c.red c.bgdark)

; UI
(add-group :CursorLine c.none c.subtle)
(add-group :LineNr c.comment)
(add-group :Normal c.fg)
(add-group :SignColumn c.comment)
(add-group :StatusLineNC c.none c.bglight)
(add-group :StatusLineTerm c.none c.bglighter s.bold)
(add-group :StatusLineTermNC c.none c.bglight)
(add-group :Statusline c.none c.bglighter s.bold)
(add-group :WildMenu c.bg c.purple s.bold)

(hi-link! :ColorColumn  :BerksBgDark)
(hi-link! :CursorColumn :CursorLine)
(hi-link! :CursorLineNr :BerksYellow)
(hi-link! :DiffAdd      :BerksGreen)
(hi-link! :DiffAdded    :DiffAdd)
(hi-link! :DiffChange   :BerksDiffChange)
(hi-link! :DiffDelete   :BerksDiffDelete)
(hi-link! :DiffRemoved  :DiffDelete)
(hi-link! :DiffText     :BerksDiffText)
(hi-link! :Directory    :BerksPurpleBold)
(hi-link! :ErrorMsg     :BerksRedInverse)
(hi-link! :FoldColumn   :BerksSubtle)
(hi-link! :Folded       :BerksBoundary)
(hi-link! :IncSearch    :BerksOrangeInverse)
(hi-link! :MoreMsg      :BerksFgBold)
(hi-link! :NonText      :BerksSubtle)
(hi-link! :Pmenu        :BerksBgDark)
(hi-link! :PmenuSbar    :BerksBgDark)
(hi-link! :PmenuSel     :BerksSelection)
(hi-link! :PmenuThumb   :BerksSelection)
(hi-link! :Question     :BerksFgBold)
(hi-link! :Search       :BerksSearch)
(hi-link! :TabLine      :BerksBoundary)
(hi-link! :TabLineFill  :BerksBgDarker)
(hi-link! :TabLineSel   :Normal)
(hi-link! :Title        :BerksGreenBold)
(hi-link! :VertSplit    :BerksBoundary)
(hi-link! :Visual       :BerksSelection)
(hi-link! :VisualNOS    :Visual)
(hi-link! :WarningMsg   :BerksOrangeInverse)

; Syntax
(add-group :MatchParen c.green c.none s.underline)
(add-group :Conceal c.cyan c.none)

(hi-link! :SpecialKey :BerksRed)
(hi-link! :LspDiagnosticsUnderline :BerksFgUnderline)
(hi-link! :LspDiagnosticsInformation :BerksCyan)
(hi-link! :LspDiagnosticsHint :BerksCyan)
(hi-link! :LspDiagnosticsError :BerksError)
(hi-link! :LspDiagnosticsWarning :BerksOrange)
(hi-link! :LspDiagnosticsUnderlineError :BerksErrorLine)
(hi-link! :LspDiagnosticsUnderlineHint :BerksInfoLine)
(hi-link! :LspDiagnosticsUnderlineInformation :BerksInfoLine)
(hi-link! :LspDiagnosticsUnderlineWarning :BerksWarnLine)

(hi-link! :Comment :BerksComment)
(hi-link! :Underlined :BerksFgUnderline)
(hi-link! :Todo :BerksTodo)

(hi-link! :Error :BerksError)
(hi-link! :SpellBad :BerksErrorLine)
(hi-link! :SpellLocal :BerksWarnLine)
(hi-link! :SpellCap :BerksInfoLine)
(hi-link! :SpellRare :BerksInfoLine)

(hi-link! :Constant :BerksPurple)
(hi-link! :String :BerksYellow)
(hi-link! :Character :BerksPink)
(hi-link! :Number :Constant)
(hi-link! :Boolean :Constant)
(hi-link! :Float :Constant)

(hi-link! :Identifier :BerksFg)
(hi-link! :Function :BerksGreen)

(hi-link! :Statement :BerksPink)
(hi-link! :Conditional :BerksPink)
(hi-link! :Repeat :BerksPink)
(hi-link! :Label :BerksPink)
(hi-link! :Operator :BerksPink)
(hi-link! :Keyword :BerksPink)
(hi-link! :Exception :BerksPink)

(hi-link! :PreProc :BerksPink)
(hi-link! :Include :BerksPink)
(hi-link! :Define :BerksPink)
(hi-link! :Macro :BerksPink)
(hi-link! :PreCondit :BerksPink)
(hi-link! :StorageClass :BerksPink)
(hi-link! :Structure :BerksPink)
(hi-link! :Typedef :BerksPink)

(hi-link! :Type :BerksCyanItalic)

(hi-link! :Delimiter :BerksFg)

(hi-link! :Special :BerksPink)
(hi-link! :SpecialComment :BerksCyanItalic)
(hi-link! :Tag :BerksCyan)
(hi-link! :helpHyperTextJump :BerksLink)
(hi-link! :helpCommand :BerksPurple)
(hi-link! :helpExample :BerksGreen)
(hi-link! :helpBacktick :Special)

; TS specific
; TODO: these should link to already defined
(add-group :TSProperty c.cyan c.none)
(add-group :TSAttribute c.green c.none)
(add-group :TSParameter c.cyan c.none)
(add-group :TSVariableBuiltIn c.red c.none)

(add-group :TSTag c.green c.none)
(add-group :TSTagDelimiter c.orange c.none)
(add-group :TSKeyword c.purple c.none)

(add-group :TSConstructor c.orange c.none)
(add-group :TSInclude c.purple c.none)
