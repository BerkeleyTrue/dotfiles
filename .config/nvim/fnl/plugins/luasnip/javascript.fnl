(module plugins.luasnip.javascript
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main [{: s : fmt : fmta : i  : c : t : sn : d : rep : ai}]
  {:log
   (s
     {:trig "log"
      :name "console log"
      :dscr "Create a console log statement."
      :wordTrig false}
     (fmta "
        console.log(\'<>\')<>"
       [(i 1 "foo")
        (c 2 [(t "") (t ";")])]))
   :logd
   (s
     {:trig "logd"
      :name "console log a var"
      :dscr "Create a console log of a var."
      :wordTrig false}
     (fmta "
        console.log(\'<>: \', <>)<>"
       [(i 1 "foo")
        (rep 1)
        (c 2 [(t "") (t ";")])]))
   :iferr
   (s
     {:trig "iferr"
      :name "if err handler"
      :dscr "Create an if-else for err first callbacks."
      :wordTrig true}
     (fmta "
       if (err) {
         return <>(err);
       }
       <>
       "
       [(i 1 "callback")
        (c 2
          [(sn nil
             (fmta "
               return <>(<>);
               "
               [(i 1 "callback")
                (i 2)]))])]))})
