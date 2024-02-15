(module plugins.mini
  {autoload
   {a aniseed.core
    r r
    md utils.module
    hl utils.highlights
    utils utils
    cl lib.color
    indent mini.indentscope
    hipatterns mini.hipatterns}
   require-macros [macros]})

(comment
  (: "[300 50 100]" :match "%[%d+ %d+ %d+%]")
  (: "[300 50 100]" :match "%[(%d+) (%d+) (%d+)%]"))
(defn main []
  (hl.link! :MiniIndentscopeSymbol :BerksSubtle)
  (indent.setup)
  (hipatterns.setup
    {:highlighters
     {:hex_color (hipatterns.gen_highlighter.hex_color)

      :hsl_vector {:pattern "%[%d+ %d+ %d+%]"
                   :group
                   (fn [_ mtch]
                     (let [(hue sat lit) (mtch:match "%[(%d+) (%d+) (%d+)%]")
                           hue (tonumber hue)
                           sat (tonumber sat)
                           lit (tonumber lit)
                           hex (cl.->hex [hue sat lit])]
                       (hipatterns.compute_hex_color_group hex "bg")))}}}))
