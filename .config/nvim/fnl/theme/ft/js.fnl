(module theme.ft.js
  {require
   {: utils
    hl utils.highlights}})

(defn main []
  (hl.link! :jsFuncCall :BerksPurple)
  (hl.link! :jsGlobalObjects :Constant)
  (hl.link! :jsGlobalNodeObjects :jsGlobalObjects)
  (hl.link! :jsThis :Constant)
  (hl.link! :jsString :BerksOrange)
  (hl.link! :jsTemplateString :BerksOrange)
  (hl.link! :jsTemplateVar :BerksYellow)
  (hl.link! :jsTemplateBraces :Comment)

  (hl.link! :jsObjectKey :BerksSapphire)
  (hl.link! :jsObject :jsObjectKey)
  (hl.link! :jsObjectProp :jsObjectKey)
  (hl.link! :jsSpreadExpression :jsObjectKey)
  (hl.link! :jsObjectKeyComputed :jsObjectKey)

  (hl.link! :jsVariableDef :jsFuncName)
  (hl.link! :jsDestructuringBlock :jsFuncName)
  (hl.link! :jsDestructuringProperty :jsFuncName)
  (hl.link! :jsDestructuringPropertyValue :jsFuncName))
