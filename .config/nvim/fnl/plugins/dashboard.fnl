(module plugin.dashboard
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(def- header
  [""
   "       , __  ______                       "
   "      /|/ \\\\(_) ||      BerkeleyTrue     "
   "       | _//    ||                        "
   "       |  \\\\  __||                       " ; extra backslash are needed to escape, this line needs to be longer to support the escaping chars
   "      _|(_// (__/                         "
   "                                          "
   ""])

(def- header2
  [
   "              ____                        "
   "       ,-'~~~~    ~~ ~ ~                  "
   "     ,'             `~~~  ',              "
   "   (                        Y             "
   "  {                          I            "
   " {      -                    `,           "
   " |       ',                   )           "
   " |        |   ,..__      __. Y            "
   " |    .,_./  Y ' / ^Y   J   )|            "
   " \\           |' /   |   |   ||           "
   "  \\          L_/    . _ (_,.'(           "
   "   \\,   ,      ^^\"\"' / |      )        "
   "     \\_  \\          /,L]     /          "
   "       '-_~-,       ` `   ./`             "
   "           `'{_            )              "
   "               ^^\\..___,.--`             "
   ""
   ""
   ""
   ""])


; create a display unit from a text and command
(defn create-button [txt sc on-press]
  {:type "button"
    :val txt
    :on_press on-press

    :opts
    {:position :center
     :hl :BerksSapphireBold
     :hl_shortcut :BerksGreen
     :align_shortcut :right
     :shortcut sc
     :cursor 0
     :width 50}})

(defn main []
  (when-let [db (md.prequire :alpha)]
    (let [dbTheme (md.prequire :alpha.themes.dashboard)
          section dbTheme.section
          button dbTheme.button
          _header {:type :text
                   :val header2
                   :opts {:position :center
                          :hl :BerksPurple}}

          buttons {:type :group
                   :val
                   [(create-button " 󱋡 " "Recently opened files" (fn [] (cmd "Telescope" {:args ["oldfiles" "cwd_only=true"]})))
                    {:type "padding" :val 1}
                    (create-button " 󰱼 " "Find File            " (fn [] (cmd "FFiles")))
                    {:type "padding" :val 1}
                    (create-button " 󰙅 " "File Browser         " (fn [] (cmd "Neotree" {:args [:float]})))
                    {:type "padding" :val 1}
                    (create-button " 󰩈 " "Quit                 " (fn [] (cmd "q!")))]}

          config {:layout
                  [{:type "padding" :val 2}
                   _header
                   {:type "padding" :val 2}
                   buttons
                   section.footer]}]

      (db.setup config))))
    ; (set db.custom_center center)
    ; (set db.custom_header header)
    ; (set db.hide_statusline false)
    ; (set db.hide_winbar false)))
