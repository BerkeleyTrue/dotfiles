(module theme.ft.markdown
  {require
   {: utils
    hl utils.highlights}})

(defn main []
  (hl.link! :markdownBlockquote        :BerksSapphire)
  (hl.link! :mkdBlockquote             :BerksSapphire)
  (hl.link! :markdownBold              :BerksOrangeBold)
  (hl.link! :markdownBoldItalic        :BerksOrangeBoldItalic)
  (hl.link! :markdownCodeBlock         :BerksGreen)
  (hl.link! :markdownCode              :BerksGreen)
  (hl.link! :markdownCodeDelimiter     :BerksOrange)
  (hl.link! :markdownH1                :BerksSapphireBold)
  (hl.link! :markdownH2                :BerksGreen)
  (hl.link! :markdownH3                :BerksOrange)
  (hl.link! :markdownH4                :BerksPink)
  (hl.link! :markdownH5                :BerksPurple)
  (hl.link! :markdownH6                :BerksYellow)
  (hl.link! :markdownHeadingDelimiter  :markdownH1)
  (hl.link! :markdownHeadingRule       :markdownH1)
  (hl.link! :markdownItalic            :BerksYellowItalic)
  (hl.link! :markdownLinkText          :BerksPink)
  (hl.link! :markdownListMarker        :BerksSapphire)
  (hl.link! :markdownOrderedListMarker :BerksSapphire)
  (hl.link! :markdownRule              :BerksComment)
  (hl.link! :markdownUrl               :BerksLink)

  ; markdown treesitter
  (hl.link "@markup.strong" :BerksOrangeBold); bold text
  (hl.link "@markup.italic" :BerksYellowItalic); italic text
  (hl.link "@markup.strikethrough" :BerksFgStrikethrough); struck-through text
  (hl.link "@markup.underline" :BerksFgUnderline); underlined text (only for literal underline markup!)

  (hl.link "@markup.heading" :BerksBlue); headings, titles (including markers)

  (hl.link! "@markup.heading.1.markdown" :BerksSapphireBold)
  (hl.link! "@markup.heading.2.markdown" :BerksGreen)
  (hl.link! "@markup.heading.3.markdown" :BerksOrange)
  (hl.link! "@markup.heading.4.markdown" :BerksPink)
  (hl.link! "@markup.heading.5.markdown" :BerksPurple)
  (hl.link! "@markup.heading.6.markdown" :BerksYellow)

  (hl.link! "@markup.heading.1.marker" :BerksSapphireBold)
  (hl.link! "@markup.heading.2.marker" :BerksGreen)
  (hl.link! "@markup.heading.3.marker" :BerksOrange)
  (hl.link! "@markup.heading.4.marker" :BerksPink)
  (hl.link! "@markup.heading.5.marker" :BerksPurple)
  (hl.link! "@markup.heading.6.marker" :BerksYellow)

  (hl.link "@markup.quote" :BerksSapphire); block quotes
  (hl.link "@markup.math" :BerksBlue); math environments (e.g. `$ ... $` in LaTeX)
  (hl.link "@markup.environment" :BerksPink); environments (e.g. in LaTeX)
  (hl.link "@markup.environment.name" :BerksBlue); environments (e.g. in LaTeX)

  (hl.link "@markup.link" :Tag); text references, footnotes, citations, etc.
  (hl.link "@markup.link.url" :BerkeRoseWatter); URL-style links

  (hl.link "@markup.raw" :BerksTeal); literal or verbatim text (e.g. inline code)

  (hl.link "@markup.list" :Special); list markers
  (hl.link "@markup.list.checked" :BerksGreen); checked todo-style list markers
  (hl.link "@markup.list.unchecked" :BerksOverlay1)); unchecked todo-style list markers
