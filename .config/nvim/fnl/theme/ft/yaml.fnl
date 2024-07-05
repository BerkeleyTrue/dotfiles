(module theme.ft.yaml
  {require
   {: utils
    hl utils.highlights}})

(defn main []
  (hl.link! :yamlAlias           :BerksGreenItalicUnderline)
  (hl.link! :yamlAnchor          :BerksPinkItalic)
  (hl.link! :yamlBlockMappingKey :BerksSapphire)
  (hl.link! :yamlFlowCollection  :BerksPink)
  (hl.link! :yamlFlowIndicator   :Delimiter)
  (hl.link! :yamlNodeTag         :BerksPink)
  (hl.link! :yamlPlainScalar     :BerksYellow))
