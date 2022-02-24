(module plugins.luasnip.fennel
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn main [{: s : fmt : fmta : i  : c : t : sn : d}]
  {:module
   (s
     {:trig "(module)"
      :name "aniseed module"
      :dscr "create an aniseed module expression"
      :wordTrig false}
     (fmta "
        (module <>
          {require
           {a aniseed.core
            r r
            md utils.module
            utils utils<>}
           require-macros [macros]})
        <>"
       [(i 1 "namespace") (i 2) (i 0)]))

   :main
   (s
     {:trig "(main)"
      :name "main func"
      :dscr "Create an main function."
      :wordTrig false}
     (fmta "
       (<> main []<>)
       "
       [(c 1 [(t "defn") (t "fn")])
        (i 0)]))

   :let
   (s
     {:trig "(let)"
      :name "binding expr"
      :dscr "Create a let binding expression."
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
      :dscr "Create a function expresssion."
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
        (i 0)]))

   :package
   (s
     {:trig "package"
      :name "Packer Package"
      :dscr "Create a packer package entry."
      :wordTrig false}
     (fmta
       "{:name :<> :description \"<>\"<>}"
       [(i 1 "name")
        (i 2 "a description")
        (c 3
           [(t "")
            (sn 1
              (fmta
                " :requires [[:<>]]"
                [(i 1 "dep")]
                {:dedent false}))])]))
   :tapx
   (s
     {:trig "tapx"
      :name "Tap a var to x"
      :dscr "Create a packer package entry."
      :wordTrig false}
     (fmta
       "(tap #(<> <> $))"
       [(c 1
           [(t "print")
            (t "a.pr")])
        (c 2
           [(t ":x")
            (t ":y")
            (t ":z")
            (t ":a")
            (t ":b")
            (t ":c")])]))})
