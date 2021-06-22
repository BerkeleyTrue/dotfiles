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
          ["--coin", "uni"]
          "0.xx"
          "uni"
          10
      , Run
          ComX
          "/home/berkeleytrue/.local/bin/crypto"
          ["--coin", "sol"]
          "0.xx"
          "sol"
          10
      , Run UnsafeStdinReader
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
      "\
    \%UnsafeStdinReader% \
    \ } \
    \<fc=#ff79c6> %date% </fc>\
    \<fc=#8be9fd> ETH $%eth% </fc>\
    \<fc=#8be9fd> %eth-gas% </fc>\
    \ { \
    \<fc=#ffb86c> BTC $%btc% </fc>\
    \<fc=#bd93f9> DPI $%dpi% </fc>\
    \<fc=#ff79c6> UNI $%uni% </fc>\
    \<fc=#ff5555> SOL $%sol% </fc>\
    \ "
  }
