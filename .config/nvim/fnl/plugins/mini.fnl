(module plugins.mini
  {autoload
   {a aniseed.core
    r r
    md utils.module
    hl utils.highlights
    utils utils
    cl lib.color
    p theme.palette
    indent mini.indentscope
    hipatterns mini.hipatterns
    pairs* mini.pairs
    move mini.move
    cmmnt mini.comment
    align mini.align}
   require-macros [macros]})

(comment
  (: "[300 50 100]" :match "%[%d+ %d+ %d+%]")
  (: "[300 50 100]" :match "%[(%d+) (%d+) (%d+)%]")
  (: "hx.red" :match "hx%.%w+")
  (: "hex.red" :match "he?x%.%w+")
  (: "p.green" :match "p%.%w+")
  (: "%#BerksStatusLineMod#" :match "Berks%w+")
  (: ":BerksStatusLine" :match "Berks%w+")
  (: "#FFF" :match "#(%x)(%x)(%x)"))

(defn main []
  (hl.link! :MiniIndentscopeSymbol :BerksSubtle)
  (indent.setup)
  (hipatterns.setup
    {:highlighters
     {:hex_color (hipatterns.gen_highlighter.hex_color)
      :hex_three_val {:pattern "#%x%x%x%f[%X]"
                      :group
                      (fn [_ mtch]
                        (let [(red grn blu) (mtch:match "#(%x)(%x)(%x)")]
                          (hipatterns.compute_hex_color_group (string.format "#%s%s%s%s%s%s" red red grn grn blu blu) "fg")))}

      :hsl_vector {:pattern "%[()%d+ %d+ %d+()%]"
                   :group
                   (fn [buf mtch]
                     (when (= (. (bo buf) :filetype) :fennel)
                       (let [(hue sat lit) (mtch:match "(%d+) (%d+) (%d+)")
                             hue (tonumber hue)
                             sat (tonumber sat)
                             lit (tonumber lit)
                             hex (cl.->hex [hue sat lit])]
                         (hipatterns.compute_hex_color_group hex "fg"))))}
      :palette_hx {:pattern "he?x%.%w+"
                   :group
                   (fn [buf mtch]
                     (when (= (. (bo buf) :filetype) :fennel)
                       (let [hx (mtch:match "he?x%.(%w+)")]
                         (when-let [hex (. p.hex hx)]
                           (hipatterns.compute_hex_color_group hex "fg")))))}
      :palette_p {:pattern "p%.%w+"
                  :group
                  (fn [buf mtch]
                    (when (= (. (bo buf) :filetype) :fennel)
                      (let [hx (mtch:match "p%.(%w+)")]
                        (when-let [hex (. p.hex hx)]
                          (hipatterns.compute_hex_color_group hex "fg")))))}
      :theme_hls {:pattern "Berks%w+"
                  :group
                  (fn [buf mtch]
                    (when (= (. (bo buf) :filetype) :fennel)
                      mtch))}}})
  (pairs*.setup)
  (move.setup)
  (cmmnt.setup
    {:mappings
     {:comment_line "<C-_>"
      :comment_visual "<C-_>"}})
  (align.setup))
