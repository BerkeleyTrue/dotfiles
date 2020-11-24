(module theme.ft.spell)

(defn main [{:add-group add-group :c c :s s}]
  (add-group :SpellBad c.none c.none s.underline)
  (add-group :SpellLocal c.none c.none s.underline)
  (add-group :SpellRare c.none c.none s.underline))
