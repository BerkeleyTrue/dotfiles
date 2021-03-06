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

Config {
  font = "xft:FiraCode Nerd Font:pixelsize=11:antialias=true:hinting=true"
  , additionalFonts = [
    "xft:Font Awesome 5 Free:style=Solid:size=10;-1"
    , "xft:coins:style=Regular;0"
  ]
  , bgColor = "#282a36"
  , fgColor = "#f8f8f2"
  , position = TopW L 100
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = False
  , persistent = True
  , commands = [
      Run Cpu ["-t", "\xf108  cpu: (<total>%)","-H","50","--high","red"] 20
    , Run Date "\xf133 %a %b %d | Week %V  %H:%M:%S " "date" 10
    , Run Network "enp5s0" ["-t", "\xf0aa  <rx>kb  \xf0ab  <tx>kb"] 20
    , Run Memory ["-t", "\xf233  mem: <used>M (<usedratio>%)"] 20
    , Run DiskU [("/", "\xf0c7  hdd: <free>")] [] 60
    , Run UnsafeStdinReader
    , Run ComX "/home/berkeleytrue/.config/xmobar/gen-trayer-padding.sh" ["panel"] "Foo" "trayerpad" 10
    , Run ComX "wakatime" ["--today"] "0" "" 10
    , Run ComX "/home/berkeleytrue/.config/polybar/timew-duration.sh" [] "0" "timewarrior" 10
  ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "\
    \%UnsafeStdinReader% \
    \ } \
    \<fc=#ff79c6> %date% </fc>\
    \<fc=#50fa7b>  %wakatime% </fc>\
    \<fc=#BD93F9>  %timewarrior% </fc>\
    \ { \
    \<fc=#666666>|</fc><fc=#ffb86c> %cpu% </fc>\
    \<fc=#666666>|</fc><fc=#ff5555> %memory% </fc>\
    \<fc=#666666>|</fc><fc=#6272a4> %disku% </fc>\
    \ %trayerpad% \
    \ "
}
