(module plugins.colors
  {:require {a aniseed.core
             nvim aniseed.nvim
             str aniseed.string}})


(def none "NONE")
(def palette {:fg        ["#F8F8F2" 253]
              :bg        ["#282A36" 236]
              :bglighter ["#424450" 238]
              :bglight   ["#343746" 237]
              :bgdark    ["#21222C" 235]
              :bgdarker  ["#191A21" 234]

              :comment   ["#6272A4"  61]
              :selection ["#44475A" 239]
              :subtle    ["#424450" 238]

              :cyan      ["#8BE9FD" 117]
              :green     ["#50FA7B"  84]
              :orange    ["#FFB86C" 215]
              :pink      ["#FF79C6" 212]
              :purple    ["#BD93F9" 141]
              :red       ["#FF5555" 203]
              :yellow    ["#F1FA8C" 228]
              :none      [none      none]})

(defn hi-link [from to]
  (nvim.ex.highlight :link from to))

(defn highlight [scope fg bg attrs special]
  (let [bg (or bg (a.get palette :none))

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


(highlight :TSProperty palette.cyan)
(highlight :TSAttribute palette.cyan)
(highlight :TSParameter palette.cyan)

(hi-link :TSTag :DraculaGreen)
(hi-link :TSTagDelimiter :DraculaOrange)
(hi-link :TSKeyword :DraculaPurple)

(highlight :TSConstructor palette.cyan)

  ; TSError: "error_red"
  ; TSPunctDelimiter: "white"
  ; TSPunctBracket: "white"
  ; TSPunctSpecial: "white"
  ; # Constants
  ; TSConstant: "yellow"
  ; TSConstBuiltin: "blue"
  ; # Not sure about this guy
  ; TSConstMacro: "cyan"
  ; TSStringRegex: "orange"
  ; TSString: "orange"
  ; TSStringEscape: "dark_yellow"
  ; TSCharacter: "orange"
  ; TSNumber: "light_green"
  ; TSBoolean: "blue"
  ; TSFloat: "light_green"
  ; TSAnnotation: "yellow"
  ; TSNamespace: "#FF00FF"
  ; # Functions
  ; TSFuncBuiltin: "yellow"
  ; TSFunction: "yellow"
  ; TSFuncMacro: "yellow"
  ; TSParameter: "light_blue"
  ; TSParameterReference: "light_blue"
  ; TSMethod: "yellow"
  ; TSField: "light_blue"
  ; TSProperty: "light_blue"
  ; # Keywords
  ; TSConditional: "purple"
  ; TSRepeat: "purple"
  ; TSLabel: "light_blue"
  ; # Does not work for yield and return they should be diff then class and def
  ; TSKeyword: "blue"
  ; TSKeywordFunction: "purple"
  ; TSKeywordOperator: "blue"
  ; TSOperator: "white"
  ; TSException: "purple"
  ; TSType: "cyan"
  ; TSTypeBuiltin: "blue"
  ; TSStructure: "#FF00FF"
  ; TSInclude: "purple"
  ; # Variable
  ; TSVariable: "light_blue"
  ; TSVariableBuiltin: "light_blue"
  ; # Text
  ; TSText: "#FFFF00"
  ; TSStrong: "#FFFF00"
  ; TSEmphasis: "#FFFF00"
  ; TSUnderline: "#FFFF00"
  ; TSTitle: "#FFFF00"
  ; TSLiteral: "#FFFF00"
  ; TSURI: "#FFFF00"
  ; # Tags
  ; TSTag: "blue"
  ; TSTagDelimiter: "line_grey"
  ; # -- END Treesitter --)


{:hi-link hi-link
 :highlight highlight
 :palette palette
 :none none}
