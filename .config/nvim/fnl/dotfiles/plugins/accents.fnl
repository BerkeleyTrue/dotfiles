(module dotfiles.plugins.accents
  {:require {a aniseed.core
             str aniseed.string
             nvim aniseed.nvim
             nutils aniseed.nvim.util
             utils dotfiles.utils
             r dotfiles.r}})


(defonce accents
  [["a" "ä" "à" "â"]
   ["A" "Ä" "À" "Â"]
   ["e" "é" "è" "ê" "ë"]
   ["E" "É" "Ê" "È"]
   ["i" "î" "ï"]
   ["I" "Î"]
   ["o" "ö" "ô"]
   ["O" "Ö" "Ô"]
   ["s" "ß"]
   ["u" "ü" "û" "ù"]
   ["U" "Ü" "Û" "Ù"]])


(defn cycle []
  "cycle through the accents
  replacing them as you iterate"
  (let [char (utils.get-char-under-curs)]
    (-?>> accents
          ; find the list with the char
          (r.find #(r.find (r.is-equal char) $1))
          ; find the next index in the list
          ; cycling back to the beginning of the list
          (#(-?>> $1
                  (r.find-index (r.is-equal char))
                  (+ 1)
                  ((fn [index] (if
                                 ; make sure next index is not out of bounds
                                 (> index (length $1)) (% index (length $1))
                                 index)))
                  ; get next char in list
                  (. $1)))
          ; prep command
          (.. "r")
          ; execute replace
          (nvim.ex.normal_))))

(defn accent-completion [findstart base]
  "completion for accents: get a list of the corresponding accents for a char"
  (if (not= findstart 0)
    (let [char (utils.get-char-under-curs)]
      (->> accents
           (r.find-index #(r.some (r.is-equal "Ü") $1))
           (#(if
               (not= $ -1) (- (nvim.fn.col ".") 2)
               -3))))

    (->> accents
         (r.find #(r.some (r.is-equal base) $1))
         (r.default-to []))))

(nutils.fn-bridge :AccentCompletion :dotfiles.plugins.accents :accent-completion)
(nutils.fn-bridge :AccentCycle :dotfiles.plugins.accents :cycle)

(defn accents-settings []
  "set up for accents"
  (nvim.buf_set_option 0 :completefunc :AccentCompletion)
  (utils.nnoremap
    "gax"
    ":call AccentCycle()<CR>"
    {:silent true :buffer true}))

(nutils.fn-bridge :AccentSettings :dotfiles.plugins.accents :accents-settings)

(do
  (nvim.ex.augroup :accents-group)
  (nvim.ex.autocmd_)
  (nvim.ex.autocmd "FileType markdown :call AccentSettings()")
  (nvim.ex.augroup :END)
  {:accent-completion accent-completion
   :cycle cycle
   :accents-settings accents-settings})
