(module theme.syntax
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    p theme.palette
    cl lib.color}
   require {}
   require-macros [macros]})

(defn main []
  (set-hl-link :Array :BerksLavendar) 
  (set-hl-link :Boolean :Constant)
  (set-hl-link :Character :BerksTeal)
  (set-hl-link :Comment :BerksOverlay0)
  (set-hl-link :Conditional :BerksPink)
  (set-hl-link :Constant :BerksPurple)
  (set-hl-link :Debug :Special)
  (set-hl-link :Define :PreProc)
  (set-hl-link :Delimiter :BerksOverlay2)

  (set-hl      :DiffAdd {:bg (cl.->hex [96 44 68])})
  (set-hl-link :DiffChange :BerksDiffChange) 
  (set-hl-link :DiffDelete :BerksDiffDelete)
  (set-hl-link :DiffText :BerksDiffText)

  (set-hl-link :diffAdded :BerksGreen)
  (set-hl-link :diffChanged :BerksSapphire)
  (set-hl-link :diffFile :BerksSapphire)
  (set-hl-link :diffIndexLine :BerksTeal)
  (set-hl-link :diffLine :BerksOverlay0)
  (set-hl-link :diffNewFile :BerksPeach)
  (set-hl-link :diffOldFile :BerksYellow)
  (set-hl-link :diffRemoved :BerksRed)

  (set-hl-link :Error :BerksRed)
  (set-hl-link :Exception :BerksPink)
  (set-hl-link :Float :Constant)
  (set-hl-link :Function :BerksGreen)

  (set-hl-link :Identifier :BerksFlamingo)
  (set-hl-link :Include :BerksMauve)
  (set-hl-link :Keyword :BerksMauve)
  (set-hl-link :Label :BerksSapphire)
  (set-hl-link :Macro :BerksMauve)
  (set-hl-link :Namespace :BerksPurple)
  (set-hl-link :Number :Constant)
  (set-hl-link :Operator :BerksSky)
  (set-hl-link :PreCondit :PreProc)
  (set-hl-link :PreProc :BerksPink)
  (set-hl-link :Repeat :BerksPink)
  (set-hl-link :Special :BerksPink)
  (set-hl-link :SpecialChar :Special)
  (set-hl-link :SpecialComment :Special)
  (set-hl-link :Statement :BerksMauve)
  (set-hl-link :StorageClass :BerksYellow)
  (set-hl-link :String :BerksYellow)
  (set-hl-link :Structure :BerksYellow)
  (set-hl-link :Tag :Special)
  (set-hl-link :Type :BerksSapphire)
  (set-hl-link :Typedef :Type)

  (set-hl :debugBreakpoint {:fg p.hex.overlay0 :bg p.hex.base})
  (set-hl :debugPC {:bg p.hex.crust})

  (set-hl-link :healthError :BerksError)
  (set-hl-link :healthSuccess :BerksTeal)
  (set-hl-link :healthWarning :BerksYellow)

  (set-hl-link :rainbow1 :BerksRed)
  (set-hl-link :rainbow2 :BerksPeach)
  (set-hl-link :rainbow3 :BerksYellow)
  (set-hl-link :rainbow4 :BerksGreen)
  (set-hl-link :rainbow5 :BerksSapphire)
  (set-hl-link :rainbow6 :BerksLavender))
