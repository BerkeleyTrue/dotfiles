(module plugins.startify
  {:require {: r
             : utils}
   :require-macros [macros]})


(defn sig []
  (utils.fn.startify#pad
    (r.concat
      ["*--------------------------------------->>"
       "|      , __  ______   "
       "|     /|/ \\\\(_) ||      BerkeleyTrue "
       "|      | _//    ||    "
       "|      |  \\\\  __||  "
       "|     _|(_// (__/    "
       "|"]
      (utils.fn.startify#fortune#cowsay))))

(defn main []
  (utils.set-nvim-g!
    {:startify_custom_header (utils.viml->luaexp *module-name* (sym->name sig))
     :startify_change_to_dir 0
     :startify_files_number 15
     :startify_lists
     [{:header ["         last opened"]
       :type :dir}]}))
