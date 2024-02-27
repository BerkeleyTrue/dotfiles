(module lib.corpus.cmp
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ftdetect lib.corpus.ftdetect}
   require {}
   require-macros [macros]})

(def- the-regx "\\[\\zs[^\\]]*\\ze\\]\\?")

(defn create-source []
  {:is_available #(ftdetect.ftdetect)
   :get_debug_name #:corpus
   :get_keyword_pattern #the-regx
   :get_trigger_characters #["["]

   :complete
   (fn [self params callback]
     (print :called)
     (let [input params.context.cursor_before_line
           search (input:sub params.offset)]
       (a.println "search" search (r.not-empty? search))
       (if (r.not-empty? search)
         (callback
           {:items
            [{:label "Some reference" :kind 18 :documentation "Some documentation of the ref"}]
            :isIncomplete true})
         (callback {:items [{:label :foo}]
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
