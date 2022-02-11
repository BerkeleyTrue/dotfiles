(module theme.ft.yaml
  {require
   {: utils
    hl utils.highlights}})

(defn main []
  (hl.link! :yamlAlias           :BerksGreenItalicUnderline)
  (hl.link! :yamlAnchor          :BerksPinkItalic)
  (hl.link! :yamlBlockMappingKey :BerksCyan)
  (hl.link! :yamlFlowCollection  :BerksPink)
  (hl.link! :yamlFlowIndicator   :Delimiter)
  (hl.link! :yamlNodeTag         :BerksPink)
  (hl.link! :yamlPlainScalar     :BerksYellow)
  (hl.link! :jsFuncCall :BerksPurple)
  (hl.link! :jsGlobalObjects :Constant)
  (hl.link! :jsGlobalNodeObjects :jsGlobalObjects)
  (hl.link! :jsThis :Constant)
  (hl.link! :jsString :BerksOrange)
  (hl.link! :jsTemplateString :BerksOrange)
  (hl.link! :jsTemplateVar :BerksYellow)
  (hl.link! :jsTemplateBraces :Comment)

  (hl.link! :jsObjectKey :BerksCyan)
  (hl.link! :jsObject :jsObjectKey)
  (hl.link! :jsSpreadExpression :jsObjectKey)
  (hl.link! :jsObjectKeyComputed :jsObjectKey)
  (hl.link! :jsObjectProp :jsObjectKey)

  (hl.link! :jsVariableDef :jsFuncName)
  (hl.link! :jsDestructuringBlock :jsFuncName)
  (hl.link! :jsDestructuringPropertyValue :jsFuncName)
  (hl.link! :jsDestructuringProperty :jsFuncName))
