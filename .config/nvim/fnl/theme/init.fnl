(module theme
  {require
   {: r
    : utils
    a aniseed.core
    hl utils.highlights
    str aniseed.string}
   require-macros [macros]})

(comment (set log.level "debug"))

(defn main [{: add-group : c : s}]

  (add-group :BerksNone c.none c.none)

  (add-group :BerksBg c.none c.bg)
  (add-group :BerksBgLight c.none c.bglight)
  (add-group :BerksBgLighter c.none c.bglighter)
  (add-group :BerksBgDark c.none c.bgdark)
  (add-group :BerksBgDarker c.none c.bgdarker)

  (add-group :BerksFg c.fg)
  (add-group :BerksFgUnderline c.fg c.none s.underline)
  (add-group :BerksFgBold c.fg c.none s.bold)

  (add-group :BerksComment c.comment)
  (add-group :BerksCommentInverse c.bg c.comment)
  (add-group :BerksCommentBold c.comment c.none s.bold)

  (add-group :BerksSelection c.none c.selection)

  (add-group :BerksSubtle c.subtle)
  (add-group :BerksGray c.gray)

  (add-group :BerksCyan c.cyan)
  (add-group :BerksCyanLight (c.cyan:light))
  (add-group :BerksCyanBold c.cyan c.none s.bold)
  (add-group :BerksCyanDark (c.cyan:dark))
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
  (add-group :BerksPinkDark c.pink c.bgdark)

  (add-group :BerksPurple c.purple)
  (add-group :BerksPurpleLight (c.purple:light))
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
  (add-group :BerksUnderline c.none c.none s.undercurl)

  (add-group :BerksDiffChange c.orange c.none)
  (add-group :BerksDiffText c.bg c.orange)
  (add-group :BerksDiffDelete c.red c.bgdark)
  (add-group :YellowToRedInverse c.red c.bg)

  (hl.link! :ColorColumn  :BerksBgDark)
  (hl.link! :DiffAdd      :BerksGreen)
  (hl.link! :DiffAdded    :DiffAdd)
  (hl.link! :DiffChange   :BerksDiffChange)
  (hl.link! :DiffDelete   :BerksDiffDelete)
  (hl.link! :DiffRemoved  :DiffDelete)
  (hl.link! :DiffText     :BerksDiffText)
  (hl.link! :Directory    :BerksPurpleBold)
  (hl.link! :ErrorMsg     :BerksRedInverse)
  (hl.link! :FoldColumn   :BerksSubtle)
  (hl.link! :Folded       :BerksBoundary)
  (hl.link! :IncSearch    :BerksOrangeInverse)
  (hl.link! :MoreMsg      :BerksFgBold)
  (hl.link! :NonText      :BerksSubtle)
  (hl.link! :Pmenu        :BerksBgDark)
  (hl.link! :PmenuSbar    :BerksBgDark)
  (hl.link! :PmenuSel     :BerksSelection)
  (hl.link! :PmenuThumb   :BerksSelection)
  (hl.link! :Question     :BerksFgBold)
  (hl.link! :Search       :BerksSearch)
  (hl.link! :TabLine      :BerksBoundary)
  (hl.link! :TabLineFill  :BerksBgDarker)
  (hl.link! :TabLineSel   :Normal)
  (hl.link! :Title        :BerksGreenBold)
  (hl.link! :VertSplit    :BerksBoundary)
  (hl.link! :Visual       :BerksSelection)
  (hl.link! :VisualNOS    :Visual)
  (hl.link! :WarningMsg   :BerksOrangeInverse)

  ; Diagnostic
  (hl.link! :DiagnosticError :Error)

  (hl.link! :DiagnosticWarn :BerksWarn)
  (hl.link! :DiagnosticFloatingWarn :BerksCyan)
  (hl.link! :DiagnosticVirtualTextWarn :Comment)
  (hl.link! :DiagnosticVirtualTextInfo :Comment)

  (hl.link! :DiagnosticInfo :BerksCyan)
  (hl.link! :DiagnosticHint :Comment)

  ; Floats!

  (hl.link! :NormalFloat :Normal)
  (hl.link! :FloatBorder :Comment)

  ; Syntax
  (add-group :MatchParen c.cyan c.none)
  (add-group :Conceal c.cyan c.none)

  (hl.link! :SpecialKey :BerksRed)

  (hl.link! :Comment :BerksComment)
  (hl.link! :Underlined :BerksFgUnderline)
  (hl.link! :Todo :BerksTodo)

  (hl.link! :Error :BerksError)
  (hl.link! :SpellBad :BerksErrorLine)
  (hl.link! :SpellLocal :BerksWarnLine)
  (hl.link! :SpellCap :BerksInfoLine)
  (hl.link! :SpellRare :BerksInfoLine)

  (hl.link! :Constant :BerksPurple)
  (hl.link! :String :BerksYellow)
  (hl.link! :Character :BerksPink)
  (hl.link! :Number :Constant)
  (hl.link! :Boolean :Constant)
  (hl.link! :Float :Constant)

  (hl.link! :Identifier :BerksFg)
  (hl.link! :Function :BerksGreen)

  (hl.link! :Statement :BerksPink)
  (hl.link! :Conditional :BerksPink)
  (hl.link! :Repeat :BerksPink)
  (hl.link! :Label :BerksPink)
  (hl.link! :Operator :BerksPink)
  (hl.link! :Keyword :BerksPink)
  (hl.link! :Exception :BerksPink)

  (hl.link! :PreProc :BerksPink)
  (hl.link! :Include :BerksPink)
  (add-group :Namespace (c.purple:light))
  (hl.link! :Define :BerksPink)
  (hl.link! :Macro :BerksPink)
  (hl.link! :PreCondit :BerksPink)
  (hl.link! :StorageClass :BerksPink)
  (hl.link! :Structure :BerksPink)
  (hl.link! :Typedef :BerksPink)

  (hl.link! :Type :BerksCyanItalic)

  (hl.link! :Delimiter :BerksFg)

  (hl.link! :Special :BerksPink)
  (hl.link! :SpecialComment :BerksCyanItalic)
  (hl.link! :Tag :BerksCyan)
  (hl.link! :helpHyperTextJump :BerksLink)
  (hl.link! :helpCommand :BerksPurple)
  (hl.link! :helpExample :BerksGreen)
  (hl.link! :helpBacktick :Special)

  ; Cursor Line
  (add-group :CursorColumn (c.bg:negative) (c.bg:light))
  (add-group :CursorLine c.none (c.bg:light))
  (hl.link! :CursorLineNr :BerksPurple)

  ; make the highlighting of tabs and other non-text less annoying
  (add-group :NonText c.gray)
  (add-group :SpecialKey (c.red:dark))

  ; TS specific
  ; TODO: these should link to already defined
  (add-group :TSProperty c.cyan c.none)
  (add-group :TSAttribute c.green c.none)
  (add-group :TSParameter c.cyan c.none)
  (add-group :TSVariableBuiltIn c.red c.none)

  (hl.link! :TSTag :Tag)
  (add-group :TSTagDelimiter c.pink c.none)
  (add-group :TSKeyword c.purple c.none)

  (add-group :TSConstructor c.orange c.none)
  (hl.link! :TSNamespace :Namespace)

  ; UI
  (add-group :LineNr c.comment)
  (add-group :Normal c.fg)
  (add-group :SignColumn c.comment)
  (add-group :Statusline c.none c.bglighter s.bold)
  (add-group :StatusLineNC c.none c.bglight)
  (add-group :StatusLineTerm c.none c.bglighter s.bold)
  (add-group :StatusLineTermNC c.none c.bglight)
  (add-group :WinBar c.none c.bglighter s.bold)
  (add-group :BerksStatusLineRed c.red c.bglighter)
  (add-group :BerksStatusLineRedInverse c.bglighter c.red)
  (add-group :BerksStatusLineInfo c.cyan c.bglighter)
  (add-group :BerksStatusLineInfoInverse c.bglighter c.cyan)
  (add-group :WildMenu c.bg c.purple s.bold)

  (run-main :theme.ft theme-methods))
