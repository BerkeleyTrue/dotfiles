(module theme.ft.spell
  {require
   {: utils
    hl utils.highlights}})

(defn main []
  (hl.link! :SpellBad :BerksUnderline)
  (hl.link! :SpellLocal :BerksUnderline)
  (hl.link! :SpellRare :BerksUnderline))
