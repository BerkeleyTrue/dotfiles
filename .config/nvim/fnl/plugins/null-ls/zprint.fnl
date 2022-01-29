(module plugins.null-ls.zprint
  {:require
   {a aniseed.core
    md utils.module}})

(defn on-output [params done]
  (let [output (.. "\n" (. params :output))]
    (done
      (if
        output [{:text (: output :gsub "\n[^\n\r]*[\n\r]+" "" 1)}]))))


(defn main [null-ls]
  (let [methods (. null-ls :methods)
        helpers (md.prequire :null-ls.helpers)
        generator (helpers.generator_factory
                    {:command "zprint-clj"
                     :args [:--hang :-i :$FILENAME]
                     :format :raw
                     :use_cache true
                     :ignore_stderr true
                     :on_output on-output})]


    {:name "zprint-clj"
     :filetypes ["clojure" "fennel"]
     :method (. methods :FORMATTING)
     :async true
     : generator}))

(comment (main (require :null-ls)))
