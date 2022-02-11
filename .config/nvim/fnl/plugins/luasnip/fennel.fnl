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
     {:trig "(module)"
      :name "aniseed module"
      :docstr "create an aniseed module expression"
      :wordTrig false}
     (fmta "
        (module <>
          {require
           {a aniseed.core
            r r
            md utils.module
            utils utils<>}
           require-macros [macros]})"
       [(i 1 "namespace") (i 2)]))

   :main
   (s
     {:trig "(main)"
      :name "main func"
      :docstr "create an aniseed main func"
      :wordTrig false}
     (fmta "
       (defn main []<>)
       "
       [(i 1)]))

   :let
   (s
     {:trig "(let)"
      :name "binding expr"
      :docstr "create a let binding"
      :wordTrig false}
     (fmta "
       (<> [<> <>]
         (<>))
       "
       [(c 1
          [(t "let")
           (t "when-let")
           (t "if-let")])
        (i 2 "foo")
        (i 3 "bar")
        (i 0 "baz")]))
   :fn
   (s
     {:trig "(fn)"
      :name "fn expr"
      :docstr "create a fn expr"
      :wordTrig true}
     (fmta "
       (<> <> []<>)
       "
       [
        (c
          1
          [(t "defn")
           (t "defn-")
           (t "fn")])
        (i 2 "name")
        (i 3)]))

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
