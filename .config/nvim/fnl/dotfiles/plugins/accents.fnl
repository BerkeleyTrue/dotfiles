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




(defn accent-completion [findstart base]
  "completion for accents: get a list of the corresponding accents for a char"
  (if (not= findstart 0)
    (let [char (utils.get-char-under-curs -2)]
      (->> accents
           (r.find-index #(r.some (r.is-equal "Ü") $1))
           (#(if
               (not= $ -1) (- (nvim.fn.col ".") 2)
               -3))))

    (->> àccents
         (r.find #(r.some (r.is-equal base) $1))
         (r.default-to []))))

(nutils.fn-bridge :AccentCompletion :dotfiles.plugins.accents :accent-completion)

(do
  (nvim.buf_set_option 0 :completefunc :AccentCompletion)
  {:accent-completion accent-completion})
