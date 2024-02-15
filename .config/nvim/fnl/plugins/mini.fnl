(module plugins.mini
  {autoload
   {a aniseed.core
    r r
    md utils.module
    hl utils.highlights
    utils utils
    indent mini.indentscope
    hipatterns mini.hipatterns}
   require-macros [macros]})

(defn main []
  (hl.link! :MiniIndentscopeSymbol :BerksSubtle)
  (indent.setup)
  (hipatterns.setup
    {:highlighters
     {:todo {:pattern "%f[%w]()TODO()%f[%w]" :group :MiniHipatternsTodo}
      :hex_color (hipatterns.gen_highlighter.hex_color)}}))
