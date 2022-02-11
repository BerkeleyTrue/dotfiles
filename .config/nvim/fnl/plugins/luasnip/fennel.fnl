(module plugins.luasnip.fennel
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main [{: fmta : i : s}]
  {:module
   (s
     {:trig "module"
      :name "aniseed module"
      :docstr "create an aniseed module expression"
      :wordTrig false}
     (fmta "
        (module <>
          {require
           {a aniseed.core
            r r
            md utils.module
            utils utils}
           require-macros [macros]})"
       [(i 1)]))
   :package
   (s
     {:trig "package"
      :wordTrig false}
     (fmta
       "{:name <> :description \"<>\"}"
       [(i 1) (i 2)]))})


(comment
  (let [fmt (. (require :luasnip.extras.fmt) :fmt)
        fmta (. (require :luasnip.extras.fmt) :fmta)
        ls (require :luasnip)
        i (. ls :i)
        s (. ls :s)
        fennel-snips (main {: fmt : fmta : s : i})]
    (a.pri fennel-snips)
    (tset ls.snippets :fennel fennel-snips)))
