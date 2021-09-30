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
      , Run ComX "/home/berkeleytrue/.local/bin/eth-gas" [] "0.xx" "eth-gas" 10
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto"
          ["--coin", "btc"]
          "0.xx"
          "btc"
          10
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto"
          ["--coin", "eth"]
          "0.xx"
          "eth"
          10
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto"
          ["--coin", "dpi"]
          "0.xx"
          "dpi"
          10
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto"
          ["--coin", "pickle"]
          "0.xx"
          "pickle"
          10
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto"
          ["--coin", "sol"]
          "0.xx"
          "sol"
          10
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto"
          ["--coin", "paxg"]
          "0.xx"
          "paxg"
          10
      , Run ComX "/home/berkeleytrue/.local/bin/spy" [] "0.xx" "spy" 10
      , Run UnsafeStdinReader
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
      "\
    \%UnsafeStdinReader% \
    \ } \
    \<fc=#ff79c6> %date% </fc>\
    \<fc=#8be9fd> ﲹ $%eth% </fc>\
    \<fc=#8be9fd> %eth-gas% </fc>\
    \ { \
    \<fc=#f1fa8c> SPY: %spy% </fc>\
    \<fc=#50fa7b>  $%btc% </fc>\
    \<fc=#ffb86c> PAXG $%paxg% </fc>\
    \<fc=#ff79c6> PICKLE $%pickle% </fc>\
    \<fc=#ff5555> SOL $%sol% </fc>\
    \<fc=#bd93f9> DPI $%dpi% </fc>\
    \ "
  }
