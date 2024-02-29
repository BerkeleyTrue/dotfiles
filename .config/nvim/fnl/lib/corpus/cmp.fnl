(module lib.corpus.cmp
  {autoload
   {a aniseed.core
    r r
    ftdetect lib.corpus.ftdetect
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
   (fn [self params callback]
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
   (fn [self item callback]
     "Resolve the completion item. This occurs write before displaying the item to the user."
     (set item.document "# No documentation available")
     (when-let [root (when-let [root (ftdetect.search item.file ".corpus")] (vf fnamemodify root ":h"))
                file (r.get-relative-path root item.file)
                content (vf readfile item.file "" 10)
                document (..
                           (r.join "\n" content) "\n"
                           "---"
                           "\n\n"
                           "*Corpus*: " file)]

       (set item.documentation document))
     (callback item))

   :execute
   (fn [self item callback]
     "Execute the completion item. This occurs after selection"
     (print "execute" item)
     (callback item))})

(defn main [cmp]
  (cmp.register_source :corpus (create-source)))
