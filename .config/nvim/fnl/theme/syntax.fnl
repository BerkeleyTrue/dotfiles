(module theme.syntax
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  {:Array {:fg C.lavender}
   :Bold {:style [:bold]}
   :Boolean {:fg C.peach :style (or O.styles.booleans {})}
   :Character {:fg C.teal}
   :Comment {:fg C.overlay0 :style O.styles.comments}
   :Conditional {:fg C.mauve :style (or O.styles.conditionals {})}
   :Constant {:fg C.peach}
   :Debug {:link :Special}
   :Define {:link :PreProc}
   :Delimiter {:fg C.overlay2}
   :DiffAdd {:bg (U.darken C.green 0.18 C.base)}
   :DiffChange {:bg (U.darken C.blue 0.07 C.base)}
   :DiffDelete {:bg (U.darken C.red 0.18 C.base)}
   :DiffText {:bg (U.darken C.blue 0.3 C.base)}
   :Error {:fg C.red}
   :Exception {:fg C.mauve :style (or O.styles.keywords {})}
   :Float {:link :Number}
   :Function {:fg C.blue :style (or O.styles.functions {})}
   :GlyphPalette1 {:fg C.red}
   :GlyphPalette2 {:fg C.teal}
   :GlyphPalette3 {:fg C.yellow}
   :GlyphPalette4 {:fg C.blue}
   :GlyphPalette6 {:fg C.teal}
   :GlyphPalette7 {:fg C.text}
   :GlyphPalette9 {:fg C.red}
   :Identifier {:fg C.flamingo :style (or O.styles.variables {})}
   :Include {:fg C.mauve :style (or O.styles.keywords {})}
   :Italic {:style [:italic]}
   :Keyword {:fg C.mauve :style (or O.styles.keywords {})}
   :Label {:fg C.sapphire}
   :Macro {:fg C.mauve}
   :Number {:fg C.peach :style (or O.styles.numbers {})}
   :Operator {:fg C.sky :style (or O.styles.operators {})}
   :PreCondit {:link :PreProc}
   :PreProc {:fg C.pink}
   :Repeat {:fg C.mauve :style (or O.styles.loops {})}
   :Special {:fg C.pink}
   :SpecialChar {:link :Special}
   :SpecialComment {:link :Special}
   :Statement {:fg C.mauve}
   :StorageClass {:fg C.yellow}
   :String {:fg C.green :style (or O.styles.strings {})}
   :Structure {:fg C.yellow}
   :Tag {:link :Special}
   :Todo {:bg C.yellow :fg C.base :style [:bold]}
   :Type {:fg C.yellow :style (or O.styles.types {})}
   :Typedef {:link :Type}
   :Underlined {:style [:underline]}
   :debugBreakpoint {:bg C.base :fg C.overlay0}
   :debugPC {:bg (or (and O.transparent_background C.none) C.crust)}
   :diffAdded {:fg C.green}
   :diffChanged {:fg C.blue}
   :diffFile {:fg C.blue}
   :diffIndexLine {:fg C.teal}
   :diffLine {:fg C.overlay0}
   :diffNewFile {:fg C.peach}
   :diffOldFile {:fg C.yellow}
   :diffRemoved {:fg C.red}
   :healthError {:fg C.red}
   :healthSuccess {:fg C.teal}
   :healthWarning {:fg C.yellow}
   :htmlH1 {:fg C.pink :style [:bold]}
   :htmlH2 {:fg C.blue :style [:bold]}
   :illuminatedCurWord {:bg C.surface1}
   :illuminatedWord {:bg C.surface1}
   :mkdCodeDelimiter {:bg C.base :fg C.text}
   :mkdCodeEnd {:fg C.flamingo :style [:bold]}
   :mkdCodeStart {:fg C.flamingo :style [:bold]}
   :qfFileName {:fg C.blue}
   :qfLineNr {:fg C.yellow}
   :rainbow1 {:fg C.red}
   :rainbow2 {:fg C.peach}
   :rainbow3 {:fg C.yellow}
   :rainbow4 {:fg C.green}
   :rainbow5 {:fg C.sapphire}
   :rainbow6 {:fg C.lavender}})
