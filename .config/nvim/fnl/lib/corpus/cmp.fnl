(module lib.corpus.cmp
  {autoload
   {a aniseed.core
    r r
    ftdetect lib.corpus.ftdetect
    refdefs lib.corpus.reference-links
    {: run} lib.spawn
    {: search} lib.corpus.search}
   require {}
   require-macros [macros]})

(def- keyword-pattern "\\[\\zs[^\\]]*\\ze\\]\\?")
(var current-job nil)

(defn create-source []
  {:is_available #(ftdetect.ftdetect (vf expand "%:p"))
   :get_debug_name #:corpus
   :get_keyword_pattern #keyword-pattern
   :get_trigger_characters #["["]

   :complete
   (fn [_self params callback]
     (when-let [cwd (ftdetect.search (vf expand "%:p") ".corpus")
                cwd (vf fnamemodify cwd ":h")
                input params.context.cursor_before_line
                input (input:sub params.offset)]
       (if (r.not-empty? input)
         (when current-job
           (current-job))

         (fn handle-results [ok? results]
           (when ok?
             (let [items (->>
                           results
                           (r.map #{:label (vf fnamemodify $ ":t:r")
                                    :kind 18
                                    :file $}))]
               (callback
                 {:items items
                  :isIncomplete false}))))

         (set current-job (search input cwd handle-results))
         (callback {:items []
                    :isIncomplete true}))))

   :resolve
   (fn [_self item callback]
     "Resolve the completion item. This occurs write before displaying the item to the user."
     (set item.documentation "# No documentation available")
     (when-let [root (ftdetect.get-corpus-root item.file)
                file (r.get-relative-path root item.file)
                content (vf readfile item.file "" 10)
                document (..
                           (r.join "\n" content) "\n"
                           "---"
                           "\n\n"
                           "*Corpus*: " file)]

       (set item.documentation document)
       (set item.destination file))
     (callback item))

   :execute
   (fn [_self item callback]
     "Execute the completion item. This occurs after selection"
     (when-let [dest item.destination
                dest (->
                       dest
                       (string.gsub "/" "" 1)
                       (string.gsub ".md" ""))]
       (refdefs.add-ref-def item.label dest))
     (callback item))})

(defn main [cmp]
  (cmp.register_source :corpus (create-source)))
