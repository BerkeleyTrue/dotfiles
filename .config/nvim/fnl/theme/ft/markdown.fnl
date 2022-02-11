(module theme.ft.markdown
  {require
   {: utils
    hl utils.highlights}})

(defn main []
  (hl.link! :markdownBlockquote        :BerksCyan)
  (hl.link! :mkdBlockquote             :BerksCyanItalic)
  (hl.link! :markdownBold              :BerksOrangeBold)
  (hl.link! :markdownBoldItalic        :BerksOrangeBoldItalic)
  (hl.link! :markdownCodeBlock         :BerksGreen)
  (hl.link! :markdownCode              :BerksGreen)
  (hl.link! :markdownCodeDelimiter     :BerksOrange)
  (hl.link! :markdownH1                :BerksCyanBold)
  (hl.link! :markdownH2                :BerksGreen)
  (hl.link! :markdownH3                :BerksOrange)
  (hl.link! :markdownH4                :BerksPink)
  (hl.link! :markdownH5                :BerksPurple)
  (hl.link! :markdownH6                :BerksYellow)
  (hl.link! :markdownHeadingDelimiter  :markdownH1)
  (hl.link! :markdownHeadingRule       :markdownH1)
  (hl.link! :markdownItalic            :BerksYellowItalic)
  (hl.link! :markdownLinkText          :BerksPink)
  (hl.link! :markdownListMarker        :BerksCyan)
  (hl.link! :markdownOrderedListMarker :BerksCyan)
  (hl.link! :markdownRule              :BerksComment)
  (hl.link! :markdownUrl               :BerksLink))
