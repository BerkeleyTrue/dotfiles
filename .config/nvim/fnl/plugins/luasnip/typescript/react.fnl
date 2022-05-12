(module plugins.luasnip.typescript.react
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main [{: s : fmt : fmta : i  : c : t : sn : d : rep : ai}]
  [;comp
   (s
     {:trig "compT"
      :name "Typescript React Comp"
      :dscr "A typescript react comp."
      :wordTrig false}
     (fmta "
        inteface Props {}
        export const <> = (props: Props) =>> {
          return ();<>
        }"
       [(i 1 "Comp")
        (i 0)]))])
