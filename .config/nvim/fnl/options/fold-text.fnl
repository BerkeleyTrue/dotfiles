(module options.fold-text
  {autoload
   {a aniseed.core
    utils utils
    r r}
   require {}
   import-macros []
   require-macros [macros]})

; parinfer does not like this file
(defn format-fold-text []
  (let [foldstart (v foldstart)
        foldend (v foldend)
        numcolwidth (+
                      (tonumber (wo foldcolumn))
                      (* (if (wo number) 1 0)
                         (wo numberwidth)))
        windowwidth (- (vf winwidth 0) numcolwidth 3)
        lines-to-fold (- foldend foldstart)

        onetab (vf strpart "          " 0 (o tabstop))]
    (->
      ; get start of fold
      foldstart
      ; get whole line of fold
      (vim.fn.getline)
      ; substitute tabs into spaces
      (vim.fn.substitute :\t onetab :g)
      ; grab the first n chars of the line
      ; n is the windowwidth - the width of the num of lines - magic number
      (vim.fn.strpart 0 (- windowwidth (vf len lines-to-fold) 5))
      (.. " 󰞘 " lines-to-fold " lines  "))))

(def format-fold-text-viml (.. (utils.viml-fn-bridge *module-name* (sym->name format-fold-text)) "()"))
