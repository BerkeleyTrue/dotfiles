(module plugins.luasnip.javascript.common
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main [{: s : fmt : fmta : i  : c : t : sn : d : rep : ai}]
  [;:log
   (s
     {:trig "log"
      :name "console log"
      :dscr "Create a console log statement."
      :wordTrig false}
     (fmta "
        console.log(\'<>\')<>"
       [(i 1 "foo")
        (c 2 [(t "") (t ";")])]))
   ;:logd
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
   ;:iferr
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
                (i 2)]))])]))

   ;:import
   (s
     {:trig "import"
      :name "Import statement"
      :dscr "Create an import statement."
      :wordTrig true}
     (fmta "
        import <> from '<>';<>"
       [(c 2
          [(sn nil (fmta
                     "{ <> }"
                     [(i 1)]))
           (i "")])
        (i 1 "module")
        (i 0)]))

   ;:require
   (s
     {:trig "require"
      :name "Require expression"
      :dscr "Create a require expression."
      :wordTrig true}
     (fmta "
        const <> = require('<>');<>"
       [(c 2
          [(sn nil (fmta
                     "{ <> }"
                     [(i 1)]))
           (t "")])
        (i 1 "module")
        (i 0)]))

   (s
     {:trig "import_"
      :name "Import lodash"
      :dscr "Create a lodash import statement."
      :wordTrig true}
     (fmta "
        import _ from 'lodash/fp';<>"
       [(i 1)]))

   (s
     {:trig "require_"
      :name "Require lodash"
      :dscr "Create a lodash require expression."
      :wordTrig true}
     (fmta "
        const _ = require('lodash/fp');<>"
       [(i 1)]))

   ;:constructor
   (s
     {:trig "const"
      :name "constructor method"
      :dscr "Create a constructor."
      :wordTrig true}
     (fmta "
       constructor(<>) {
         <>
       }"
       [(i 1 "")
        (c 2
          [(t "")
           (sn nil (fmta "
                     super();<>
                     "
                     [(i 1 "")]))])]))
   (s
     {:trig "cx"
      :name "class modules"
      :dscr "Create a class module"
      :wordTrig true}
     (fmta "
       className={cx('<>')}"
       [(i 1 "")]))])
