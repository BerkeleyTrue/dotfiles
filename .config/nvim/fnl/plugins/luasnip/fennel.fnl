(module plugins.luasnip.fennel
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn main [{: fmt : fmta : i : s : c : t}]
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

   :main
   (s
     {:trig "(main"
      :name "main func"
      :docstr "create an aniseed main func"
      :wordTrig false}
     (fmt "
       (defn main [])
       "
       []))

   :when-let
   (s
     {:trig "(when-let"
      :name "when-let expr"
      :docstr "create a when-let expr"
      :wordTrig false}
     (fmt "
       (when-let [{} {}]
         ({})
       "
       [(i 1 "foo")
        (i 2 "bar")
        (i 3 "baz")]))
   :fn
   (s
     {:trig "(fn"
      :name "fn expr"
      :docstr "create a fn expr"
      :wordTrig false}
     (fmt "
       ({} {} [{}])
       "
       [(c 1 [(t "defn") (t "defn-") (t "fn")])
        (i 2)
        (i 3 "arg")]))

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
