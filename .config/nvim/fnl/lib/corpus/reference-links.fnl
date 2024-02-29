(module lib.corpus.reference-links
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ts lib.corpus.treesitter}
   require-macros [macros]})

(defn has-ref-defs? []
  "Return true if the buffer contains reference definitions for links."
  (r.not-empty? (ts.extract-link-reference-definitions)))

(defn get-links-to-add []
  "Return a list of link shortcuts that are not already defined in the reference definitions."
  (let [shortcuts (ts.extract-link-shortcuts)]
    (when (r.not-empty? shortcuts)
      (let [refdefs (->>
                      (ts.extract-link-reference-definitions)
                      (r.map (fn [{: label}] [(r.to-lower-case label) true]))
                      (r.from-pairs))]
        (->>
          shortcuts
          (r.reject #(->> (r.to-lower-case $) (. refdefs))))))))

(defn ensure-blank-line-at-end-of-buffer []
  "Ensure that the buffer ends with a blank line."
  (when (not (has-ref-defs?))
    (vf append "$" "")))

(defn add-new-ref-defs [to-add]
  "Take a list of link shortcuts and add them to the reference definitions."
  (let [refdefs (->>
                  to-add
                  (r.map (fn [label] (.. "[" label "]: /" (r.kebab-case label) ".md"))))]
    ; while last line is not empty, delete line
    (while (r.empty? (vf getline "$"))
      (vf execute (.. (vf line "$") "d")))
    ; when the last line of the buffer is not a ref-def, add an empty line
    (ensure-blank-line-at-end-of-buffer)
    (vf append "$" refdefs)))

(defn add-ref-def [label dest]
  "Add a reference definition to the buffer, first checking that label doesn't currently exist."
  (let [refdefs (->>
                  (ts.extract-link-reference-definitions)
                  (r.map (fn [{: label}] [label true]))
                  (r.from-pairs))]

    (when-not (r.includes? refdefs label)
      (ensure-blank-line-at-end-of-buffer)
      (vf append "$" (.. "[" label "]: /" dest ".md")))))

(defn update-file []
  "Add new reference definitions to the current buffer."
  (case (get-links-to-add)
    [& refs] (add-new-ref-defs refs)
    _ (a.println "No new links to add.")))


(comment
  (command! :CorpusGetLinks (fn [] (update-file))))
