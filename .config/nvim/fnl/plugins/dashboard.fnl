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

(def- center
  [{:icon "  "
    :desc "Recently opened files                   "
    :action "OOldFiles"}
   {:icon "  "
    :desc "Find  File                              "
    :action "FFiles"}
   {:icon "  "
    :desc "File Browser                            "
    :action "NeoTreeFloat"}
   {:icon "  "
    :desc "Find  word                              "
    :action "Telescope live_grep"}
   {:icon "  "
    :desc "Quit                                    "
    :action "q!"}])

(defn main []
  (when-let [db (md.prequire :dashboard)]
    (set db.custom_center center)
    (set db.custom_header header)
    (set db.hide_statusline false)
    (set db.hide_winbar false)))
