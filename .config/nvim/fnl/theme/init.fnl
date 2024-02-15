(module theme
  {autoload
   {cb colorbuddy
    p theme.palette}
   require
   {r r
    utils utils
    a aniseed.core
    hl utils.highlights
    str aniseed.string}
   require-macros [macros]})

(comment (def c cb.colors))

(n set_hl 0 :BerksNone {:fg :none :bg :none})

(defn main [{: add-group : c : s}]
  (add-group :BerksBg c.none c.bg)
  (add-group :BerksBgLight c.none c.bglight)
  (add-group :BerksBgLighter c.none c.bglighter)
  (add-group :BerksBgDark c.none c.bgdark)
  (add-group :BerksBgDarker c.none c.bgdarker)

  (add-group :BerksFg c.text)
  (add-group :BerksFgStrikethrough c.text)
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
  (add-group :BerksOrangeDark (c.orange:dark))
  (add-group :BerksOrangeLight (c.orange:light))
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
  (add-group :BerksErrorInverse c.none c.red)

  (add-group :BerksWarn c.orange c.none)
  (add-group :BerksWarnInverse c.none c.orange)

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

  (add-group :BerksRosewater c.rosewater)
  (add-group :BerksTeal c.teal)
  (add-group :BerksOverlay1 c.overlay1)


  ; Diagnostic
  (hl.link! :DiagnosticError :Error)

  (hl.link! :DiagnosticWarn :BerksWarn)
  (hl.link! :DiagnosticFloatingWarn :BerksCyan)
  (hl.link! :DiagnosticVirtualTextWarn :Comment)
  (hl.link! :DiagnosticVirtualTextInfo :Comment)

  (hl.link! :DiagnosticInfo :BerksCyan)
  (hl.link! :DiagnosticHint :Comment)


  ; Syntax


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

  ; TS specific
  ; TODO: these should link to already defined
  (add-group :TSProperty c.cyan c.none)
  (hl.link! "@property" :TSProperty)
  (add-group :TSAttribute c.green c.none)
  (add-group :TSParameter c.cyan c.none)
  (hl.link! "@parameter" :TSParameter)
  (add-group :TSVariableBuiltIn c.red c.none)

  (hl.link! :TSTag :Tag)
  (add-group :TSTagDelimiter c.pink c.none)
  (add-group :TSKeyword c.purple c.none)
  (hl.link! "@variable.builtin" :TSVariableBuiltIn)
  (hl.link! "@keyword.operator" :Operator)

  (add-group :TSConstructor c.orange c.none)
  (hl.link! "@constructor" :TSConstructor)
  (hl.link! :TSNamespace :Namespace)

  (run-main :theme.ft {: add-group : c : s})
  (run-main :theme.editor {: add-group : c : s})

  ; Telescope Todos
  (add-group :BerksTLTodo c.cyan c.none)
  (add-group :BerksTLTodoInverse c.bg c.cyan s.bold)

  (add-group :BerksTLNote c.green c.none)
  (add-group :BerksTLNoteInverse c.bg c.green s.bold)

  (add-group :BerksTLHack c.orange c.none)
  (add-group :BerksTLHackInverse c.bg c.orange s.bold)

  (add-group :BerksTLPerf c.purple c.none)
  (add-group :BerksTLPerfInverse c.bg c.purple s.bold))
