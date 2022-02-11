(module utils.highlights
  {require
   {a aniseed.core
    r r
    str aniseed.string
    nvim aniseed.nvim}
   require-macros [macros]})

(def- none "NONE")

(defn link [from to override?]
  "create a highlight link"
  (if override? (nvim.ex.highlight_ :link from to)
      (nvim.ex.highlight :link from to)))

(defn link! [from to]
  "create a highlight! link"
  (link from to true))

(defn highlight [scope fg bg attrs special]
  "create a highlight using a term/gui palette"
  (let [bg (or bg [none none])
        fg (or fg [none none])

        attrs (str.join
                ", "
                (a.filter
                  #(= (type $1) "string")
                  (or attrs [none])))

        special (or special [none none])

        fg (if (and (not= (a.first special) none)
                    (= (a.first fg) none)
                    (not (nvim.fn.has "gui_running")))
             special
             fg)

        guifg (.. "guifg='" (a.first fg) "'")
        ctermfg (.. "ctermfg='" (a.second fg) "'")

        guibg (.. "guibg='" (a.first bg) "'")
        ctermbg (.. "ctermbg='" (a.second bg) "'")

        gui (.. "gui='" attrs "'")
        cterm (.. "cterm='" attrs "'")

        guisp (.. "guisp='" (a.first special) "'")]

    (nvim.ex.highlight scope gui cterm guifg ctermfg guibg ctermbg guisp)))

(defn hi-clear [group]
  (nvim.ex.highlight_ :clear group))
