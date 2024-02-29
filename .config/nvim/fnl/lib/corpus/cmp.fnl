(module lib.corpus.cmp
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ftdetect lib.corpus.ftdetect
    {: run} lib.spawn}
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
                search (input:sub params.offset)]
       (if (r.not-empty? search)
         (let [terms (->>
                       search
                       (r.lmatch "%S+")
                       (r.join "|"))]
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

           (set current-job
             (run {:command :ag
                   :args [:--silent
                          :--files-with-matches
                          terms
                          cwd]}
                  handle-results)))
         (callback {:items []
                    :isIncomplete true}))))

   :resolve
   (fn [self item callback]
     (print :resolve)
     "Resolve the completion item. This occurs write before displaying the item to the user."
     (callback item))

   :execute
   (fn [self item callback]
     "execute the completion item. This occurs after selection"
     (print "execute" item)
     (callback item))})

(defn main [cmp]
  (cmp.register_source :corpus (create-source)))
