(module theme.ft.spell
  {:require {: utils}})

(defn main [{: add-group : c : s}]
  (add-group :SpellBad c.none c.none s.underline)
  (add-group :SpellLocal c.none c.none s.underline)
  (add-group :SpellRare c.none c.none s.underline))
