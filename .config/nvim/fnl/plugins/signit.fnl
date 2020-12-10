;      , __  ______
;     /|/  \(_) |      BerkeleyTrue
;      | __/    |      https://github.com/berkeleytrue
;      |   \  _ |
;      |(__/ (_/
;
;
(module plugins.signit
  {:require {: utils}})

(defn main []
  (->
    {:signit_initials      :BT
     :signit_name          :BerkeleyTrue
     :signit_extra_1       :https://github.com/berkeleytrue
     :signit_extra_2       ""
     :signit_ascii_font    :script.flf
     :signit_ascii_spacing :normal}
    (utils.set-nvim-g!)))
