(module completion-sources.conjure
  {:require {: r
             : utils
             : completion
             :opts completion.option
             :cutils completion.util
             :ceval conjure.eval
             :promise conjure.promise}})


(def- kind :Conjure)

(defn getCompletionItems [prefix]
  (let [len (r.size prefix)
        score-filter #(or (= len 0) (> (/ len 3) $1))
        kind (or (. (or (opts.get_option :customize_lsp_label) {}) kind) kind)]
    (->>
      (ceval.completions-sync prefix)
      (r.map
        (fn [res]
          (r.merge {} res {:score (cutils.fuzzy_score prefix res.word) : kind})))
      (r.filter #(score-filter (. (or $1 {}) :score))))))

(completion.addCompletionSource :conjure {:item getCompletionItems})
