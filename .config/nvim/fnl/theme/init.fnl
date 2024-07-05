(module theme
  {autoload
   {cb colorbuddy
    p theme.palette
    ft theme.ft
    cl lib.color
    syntax theme.syntax
    editor theme.editor}
   require
   {r r
    utils utils
    a aniseed.core
    hl utils.highlights
    str aniseed.string}
   require-macros [macros]})

(defn main []
  (set-hl :BerksNone {:fg p.none :bg p.none})

  (set-hl :BerksBgLight {:fg p.none :bg p.hex.surface1})
  (set-hl :BerksBgLighter {:fg p.none :bg p.hex.surface0})
  (set-hl :BerksBg {:fg p.none :bg (cl.->hex p.base)})
  (set-hl :BerksBgDark {:fg p.none :bg p.hex.mantle})
  (set-hl :BerksBgDarker {:fg p.none :bg p.hex.crust})

  (set-hl :BerksFg {:fg p.hex.text})
  (set-hl :BerksFgStrikethrough {:fg p.hex.text :strikethrough true})
  (set-hl :BerksFgUnderline {:fg p.hex.text :bg p.none :underline true})
  (set-hl :BerksFgBold {:fg p.hex.text :bg p.none :bold true})

  (set-hl :BerksComment {:fg p.hex.subtext0})
  (set-hl :BerksCommentInverse {:fg p.hex.base :bg p.hex.subtext0})
  (set-hl :BerksCommentBold {:fg p.hex.subtext0 :bold true})

  (set-hl :BerksSelection {:fg p.none :bg p.hex.surface0})

  (set-hl :BerksSubtle {:fg p.hex.surface0})
  (set-hl :BerksGray {:fg p.hex.overlay2})

  (set-hl :BerksGreen {:fg p.hex.green})
  (set-hl :BerksGreenDarker {:fg (cl.->hex [96 44 60])})
  (set-hl :BerksGreenBold {:fg p.hex.green :bold true})
  (set-hl :BerksGreenItalic {:fg p.hex.green :italic true})
  (set-hl :BerksGreenItalicUnderline {:fg p.hex.green :italic true :underline true})

  (set-hl :BerksOrange {:fg p.hex.peach})
  (set-hl :BerksOrangeDark {:fg (cl.->hex [20 79 65])})
  (set-hl :BerksOrangeLight {:fg (cl.->hex [20 79 79])})
  (set-hl :BerksOrangeBold {:fg p.hex.peach :bold true})
  (set-hl :BerksOrangeItalic {:fg p.hex.peach :italic true})
  (set-hl :BerksOrangeBoldItalic {:fg p.hex.peach :bold true :italic true})
  (set-hl :BerksOrangeInverse {:fg p.hex.base :bg p.hex.peach})

  (set-hl :BerksSapphire {:fg p.hex.sapphire})
  (set-hl :BerksSapphireLight {:fg (cl.->hex [199 55 79])})
  (set-hl :BerksSapphireBold {:fg p.hex.sapphire :bold true})
  (set-hl :BerksSapphireDark {:fg (cl.->hex [199 55 60])})
  (set-hl :BerksSapphireItalic {:fg p.hex.sapphire :italic true})

  (set-hl :BerksPink {:fg p.hex.pink})
  (set-hl :BerksPinkItalic {:fg p.hex.pink :italic true})
  (set-hl :BerksPinkDark {:fg p.hex.pink :bg p.hex.mantle})

  (set-hl :BerksPurple {:fg p.hex.mauve})
  (set-hl :BerksPurpleLight {:fg (cl.->hex [276 59 82])})
  (set-hl :BerksPurpleBold {:fg p.hex.mauve :bold true})
  (set-hl :BerksPurpleItalic {:fg p.hex.mauve :italic true})

  (set-hl :BerksRed {:fg p.hex.red})
  (set-hl :BerksRedInverse {:fg p.hex.text :bg p.hex.red})

  (set-hl :BerksYellow {:fg p.hex.yellow})
  (set-hl :BerksYellowItalic {:fg p.hex.yellow :italic true})

  (set-hl-link :BerksError :BerksRed)
  (set-hl-link :BerksErrorInverse :BerksRedInverse)

  (set-hl-link :BerksWarn :BerksOrange) 
  (set-hl-link :BerksWarnInverse :BerksOrangeInverse)

  (set-hl :BerksErrorLine {:fg p.hex.red :undercurl true})
  (set-hl :BerksWarnLine {:fg p.hex.peach :undercurl true})
  (set-hl :BerksInfoLine {:fg p.hex.sapphire :undercurl true})

  (set-hl :BerksTodo {:fg p.hex.overlay0 :bg p.hex.sapphire :bold true})
  (set-hl :BerksSearch {:fg p.hex.overlay0 :bg p.hex.green :bold true})
  (set-hl :BerksBoundary {:fg p.hex.subtext0 :bg p.hex.mantle})
  (set-hl :BerksLink {:fg p.hex.sapphire :underline true})
  (set-hl :BerksUnderline {:fg p.none :undercurl true})

  (set-hl :BerksDiffChange {:fg p.hex.peach})
  (set-hl :BerksDiffText {:fg p.hex.mantle :bg p.hex.peach :bold true})
  (set-hl :BerksDiffDelete {:fg p.hex.red :bg p.hex.mantle :bold true})

  (set-hl :BerksRosewater {:fg p.hex.rosewater})
  (set-hl :BerksFlamingo {:fg p.hex.flamingo})
  (set-hl :BerksMauve {:fg p.hex.mauve})
  (set-hl :BerksMaroon {:fg p.hex.maroon})
  (set-hl :BerksPeach {:fg p.hex.peach})
  (set-hl :BerksTeal {:fg p.hex.teal})
  (set-hl :BerksSky {:fg p.hex.sky})
  (set-hl :BerksLavendar {:fg p.hex.lavender})
  (set-hl :BerksText {:fg p.hex.text})
  (set-hl :BerksOverlay2 {:fg p.hex.overlay2})
  (set-hl :BerksOverlay1 {:fg p.hex.overlay1})
  (set-hl :BerksOverlay0 {:fg p.hex.overlay0})
  (set-hl :BerksSurface2 {:fg p.hex.surface2})
  (set-hl :BerksSurface1 {:fg p.hex.surface1})
  (set-hl :BerksSurface0 {:fg p.hex.surface0})
  (set-hl :BerksBase {:fg p.hex.base})
  (set-hl :BerksMantle {:fg p.hex.mantle})
  (set-hl :BerksCrust {:fg p.hex.crust})

  (editor.main)
  (syntax.main)

  ; Syntax
  (set-hl-link :Underlined :BerksFgUnderline)
  (set-hl-link :Bold :BerksBold)

  (set-hl-link :Todo :BerksTodo)

  (set-hl-link :SpellBad :BerksErrorLine)
  (set-hl-link :SpellLocal :BerksWarnLine)
  (set-hl-link :SpellCap :BerksInfoLine)
  (set-hl-link :SpellRare :BerksInfoLine)


  (set-hl-link :helpHyperTextJump :BerksLink)
  (set-hl-link :helpCommand :BerksPurple)
  (set-hl-link :helpExample :BerksGreen)
  (set-hl-link :helpBacktick :Special)

  ; Diagnostic
  (set-hl-link :DiagnosticError :Error)

  (set-hl-link :DiagnosticWarn :BerksWarn)
  (set-hl-link :DiagnosticFloatingWarn :BerksSapphire)
  (set-hl-link :DiagnosticVirtualTextWarn :Comment)
  (set-hl-link :DiagnosticVirtualTextInfo :Comment)

  (set-hl-link :DiagnosticInfo :BerksSapphire)
  (set-hl-link :DiagnosticHint :Comment)


  (set-hl-link "@function.macro" :Macro))
