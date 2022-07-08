-- background = #282a36
-- selection = #44475a
-- foreground = #f8f8f2
-- comment = #6272a4
-- cyan = #8be9fd
-- green = #50fa7b
-- orange = #ffb86c
-- pink = #ff79c6
-- purple = #bd93f9
-- red = #ff5555
-- yellow = #f1fa8c
Config
  { font = "xft:FiraCode Nerd Font:pixelsize=11:antialias=true:hinting=true"
  , additionalFonts =
      [ "xft:Font Awesome 5 Free:style=Solid:size=10;-1"
      , "xft:coins:style=Regular;0"
      ]
  , bgColor = "#282a36"
  , fgColor = "#f8f8f2"
  , position = OnScreen 1 (BottomW L 100)
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = False
  , persistent = True
  , commands =
      [ Run Date " %H:%M:%S " "date" 5
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto-egg-go"
          ["price", "btc"]
          "0.xx"
          "btc"
          10
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto-egg-go"
          ["price", "pickle"]
          "0.xx"
          "pickle"
          600
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto-egg-go"
          ["price", "sol"]
          "0.xx"
          "sol"
          200
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto-egg-go"
          ["price", "xtz"]
          "0.xx"
          "xtz"
          50
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto-egg-go"
          ["price", "ohm"]
          "0.xx"
          "ohm"
          50
      , Run ComX "/home/berkeleytrue/.local/bin/spy" [] "0.xx" "spy" 600
      , Run UnsafeStdinReader
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
      "\
    \%UnsafeStdinReader% \
    \ } \
    \<fc=#ff79c6> %date% </fc>\
    \ { \
    \<fc=#50fa7b>  $%btc% </fc>\
    \<fc=#bd93f9>  $%ohm% </fc>\
    \<fc=#ff5555> XTZ $%xtz% </fc>\
    \<fc=#ff79c6> PICKLE $%pickle% </fc>\
    \<fc=#f1fa8c> SPY: $%spy% </fc>\
    \ "
  }
