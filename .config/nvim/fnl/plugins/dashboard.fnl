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

(def- center [{:icon "  "
               :desc "Recently opened files                   "
               :action "Telescope oldfiles"}
              {:icon "  "
               :desc "Find  File                              "
               :action "Telescope find_files find_command=rg,--hidden,--files"}
              {:icon "  "
               :desc "File Browser                            "
               :action "NeoTreeFloat"}
              {:icon "  "
               :desc "Find  word                              "
               :action "Telescope live_grep"}
              {:icon "  "
               :desc "Quit                                    "
               :action "q"}])

(defn main []
  (when-let [db (md.prequire :dashboard)]
    (set db.custom_center center)
    (set db.custom_header header)))
