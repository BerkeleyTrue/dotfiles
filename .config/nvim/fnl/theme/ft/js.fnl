(module theme.ft.js
  {:require {th theme
             utils utils}})

(defn main [{:hi-link! hi-link!}]
  [(hi-link! :jsFuncCall :BerksPurple)
   (hi-link! :jsGlobalObjects :Constant)
   (hi-link! :jsGlobalNodeObjects :jsGlobalObjects)
   (hi-link! :jsThis :Constant)
   (hi-link! :jsString :BerksOrange)
   (hi-link! :jsTemplateString :BerksOrange)
   (hi-link! :jsTemplateVar :BerksYellow)
   (hi-link! :jsTemplateBraces :Comment)

   (hi-link! :jsObjectKey :BerksCyan)
   (hi-link! :jsObject :jsObjectKey)
   (hi-link! :jsSpreadExpression :jsObjectKey)
   (hi-link! :jsObjectKeyComputed :jsObjectKey)
   (hi-link! :jsObjectProp :jsObjectKey)

   (hi-link! :jsVariableDef :jsFuncName)
   (hi-link! :jsDestructuringBlock :jsFuncName)
   (hi-link! :jsDestructuringPropertyValue :jsFuncName)
   (hi-link! :jsDestructuringProperty :jsFuncName)])
