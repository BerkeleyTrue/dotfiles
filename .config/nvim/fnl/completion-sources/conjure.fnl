(module completion-sources.conjure
  {require
   {: r
    : utils
    a aniseed.core
    :ceval conjure.eval}})

(def pattern-regex (utils.regex "[0-9a-zA-Z.!$%&*+/:<=>?#_~\\^\\-\\\\]\\+$"))
(def- source-conf
  {:priority 1000
   :filetypes [:fennel :clojure :racket :janet]
   :dub false
   :menu "[conjure]"})

(defn- determine [_ {: before_char
                     : before_line
                     : col}]
  (let [offset (pattern-regex:match_str before_line)
        has-dot (r.includes before_char "%.")
        has-slash (r.includes before_char "/")
        trigger (if (or has-dot has-slash) col 0)]

    (if
      (r.nil? offset) {}
      {:keyword_pattern_offset (+ offset 1)
       :trigger_character_offset trigger})))

(defn- complete [_ {: context
                    : keyword_pattern_offset
                    : callback}]
  (let [input (context:get_input keyword_pattern_offset)]
    (ceval.completions input (fn [items]
                              (callback {: items})))))

(def source
  {:get_metadata #source-conf
   : complete
   : determine})
