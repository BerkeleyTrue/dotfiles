(module completion-sources.tenk
  {:require {: r
             : utils
             : completion
             :opts completion.option
             :cutils completion.util}})

(defn slurp [path silent?]
  "Read the file into a string."
  (match (io.open path "r")
    (nil msg) (do (when (not silent?) (print (.. "Could not load file: " msg))) nil)
    f (let [content (f:read "*all")]
        (f:close)
        content)))

(def- kind :Tenk)
(defonce- words (r.words (slurp (utils.fn.expand "~/.config/nvim/fnl/completion-sources/tenk/words.txt") "\n")))

(defn getCompletionItems [prefix]
  (let [len (r.size prefix)
        score-filter #(or (= len 0) (> (/ len 3) $1))
        kind (or (. (or (opts.get_option :customize_lsp_label) {}) kind) kind)]
    (->>
      words
      (r.map
        (fn [word]
          {: word
           : kind
           :icase 1
           :score (cutils.fuzzy_score prefix word)}))
      (r.filter #(score-filter (. (or $1 {}) :score))))))

(completion.addCompletionSource :tenk {:item getCompletionItems})
