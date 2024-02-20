(module lib.accents
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   require-macros [macros]})

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
          (.. "normal! r")
          ; execute replace
          (vim.api.nvim_command))))

(defn completion [findstart base]
  "completion for accents: get a list of the corresponding accents for a char"
  (if (not= findstart 0)
    (let [char (utils.get-char-under-curs)]
      (->> accents
           (r.find-index #(r.some (r.is-equal "Ü") $1))
           (#(if
               (not= $ -1) (- (vf col ".") 2)
               -3))))

    (->> accents
         (r.find #(r.some (r.is-equal base) $1))
         (r.default-to []))))

(defn setup []
  "set up for accents"
  (bo! :completefunc (viml->lua* completion))
  (nnoremap :gax cycle {:silent true :buffer true}))

(defn main []
  (augroup
    :AccentsGroup
    {:event :FileType
     :pattern :markdown
     :callback setup}))
