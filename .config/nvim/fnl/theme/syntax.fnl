(module theme.syntax
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    p theme.palette}
   require {}
   require-macros [macros]})

(defn main []
  {:Array {:fg p.lavender}
   :Bold {:style [:bold]}
   :Boolean {:fg p.peach :style (or O.styles.booleans {})}
   :Character {:fg p.teal}
   :Comment {:fg p.overlay0 :style O.styles.comments}
   :Conditional {:fg p.mauve :style (or O.styles.conditionals {})}
   :Constant {:fg p.peach}
   :Debug {:link :Special}
   :Define {:link :PreProc}
   :Delimiter {:fg p.overlay2}
   :DiffAdd {:bg (U.darken p.green 0.18 p.base)}
   :DiffChange {:bg (U.darken p.blue 0.07 p.base)}
   :DiffDelete {:bg (U.darken p.red 0.18 p.base)}
   :DiffText {:bg (U.darken p.blue 0.3 p.base)}
   :Error {:fg p.red}
   :Exception {:fg p.mauve :style (or O.styles.keywords {})}
   :Float {:link :Number}
   :Function {:fg p.blue :style (or O.styles.functions {})}
   :GlyphPalette1 {:fg p.red}
   :GlyphPalette2 {:fg p.teal}
   :GlyphPalette3 {:fg p.yellow}
   :GlyphPalette4 {:fg p.blue}
   :GlyphPalette6 {:fg p.teal}
   :GlyphPalette7 {:fg p.text}
   :GlyphPalette9 {:fg p.red}
   :Identifier {:fg p.flamingo :style (or O.styles.variables {})}
   :Include {:fg p.mauve :style (or O.styles.keywords {})}
   :Italic {:style [:italic]}
   :Keyword {:fg p.mauve :style (or O.styles.keywords {})}
   :Label {:fg p.sapphire}
   :Macro {:fg p.mauve}
   :Number {:fg p.peach :style (or O.styles.numbers {})}
   :Operator {:fg p.sky :style (or O.styles.operators {})}
   :PreCondit {:link :PreProc}
   :PreProc {:fg p.pink}
   :Repeat {:fg p.mauve :style (or O.styles.loops {})}
   :Special {:fg p.pink}
   :SpecialChar {:link :Special}
   :SpecialComment {:link :Special}
   :Statement {:fg p.mauve}
   :StorageClass {:fg p.yellow}
   :String {:fg p.green :style (or O.styles.strings {})}
   :Structure {:fg p.yellow}
   :Tag {:link :Special}
   :Todo {:bg p.yellow :fg p.base :style [:bold]}
   :Type {:fg p.yellow :style (or O.styles.types {})}
   :Typedef {:link :Type}
   :Underlined {:style [:underline]}
   :debugBreakpoint {:bg p.base :fg p.overlay0}
   :debugPC {:bg (or (and O.transparent_background p.none) p.crust)}
   :diffAdded {:fg p.green}
   :diffChanged {:fg p.blue}
   :diffFile {:fg p.blue}
   :diffIndexLine {:fg p.teal}
   :diffLine {:fg p.overlay0}
   :diffNewFile {:fg p.peach}
   :diffOldFile {:fg p.yellow}
   :diffRemoved {:fg p.red}
   :healthError {:fg p.red}
   :healthSuccess {:fg p.teal}
   :healthWarning {:fg p.yellow}
   :htmlH1 {:fg p.pink :style [:bold]}
   :htmlH2 {:fg p.blue :style [:bold]}
   :illuminatedCurWord {:bg p.surface1}
   :illuminatedWord {:bg p.surface1}
   :mkdCodeDelimiter {:bg p.base :fg p.text}
   :mkdCodeEnd {:fg p.flamingo :style [:bold]}
   :mkdCodeStart {:fg p.flamingo :style [:bold]}
   :qfFileName {:fg p.blue}
   :qfLineNr {:fg p.yellow}
   :rainbow1 {:fg p.red}
   :rainbow2 {:fg p.peach}
   :rainbow3 {:fg p.yellow}
   :rainbow4 {:fg p.green}
   :rainbow5 {:fg p.sapphire}
   :rainbow6 {:fg p.lavender}})
