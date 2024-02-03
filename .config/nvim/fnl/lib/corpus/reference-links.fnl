(module lib.corpus.reference-links
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ts lib.corpus.treesitter}
   require-macros [macros]})

(defn get-links-to-add []
  (let [shortcuts (ts.extract-link-shortcuts)]
    (when (not (r.empty? shortcuts))
      (let [refdefs (->>
                      (ts.extract-link-reference-definitions)
                      (r.map (fn [{: label}] [(r.to-lower-case label) true]))
                      (r.from-pairs))]
        (->>
          shortcuts
          (r.reject #(->> (r.to-lower-case $) (. refdefs))))))))

(defn add-new-ref-defs [to-add]
  "Take a list of link shortcuts and add them to the reference definitions."
  (let [refdefs (->>
                  to-add
                  (r.map (fn [label] (.. "[" label "]: ./" (r.kebab-case label) ".md"))))]
    (vf append "$" refdefs)))

(defn update-file []
  (case (get-links-to-add)
    [& refs] (add-new-ref-defs refs)
    _ (a.println "No new links to add.")))


(comment
  (command! :CorpusGetLinks (fn [] (update-file))))
