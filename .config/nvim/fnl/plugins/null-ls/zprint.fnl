(module plugins.null-ls.zprint
  {require
   {a aniseed.core
    md utils.module}})

(defn on-output [params done]
  (let [output (. params :output)
        err (. params :err)]

    (if err
      (do
        (vim.notify "zpring could not format")
        (done))
      (done (when output [{:text output}])))))


(defn main [null-ls]
  (let [methods (. null-ls :methods)
        helpers (md.prequire :null-ls.helpers)
        generator (helpers.generator_factory
                    {:command "zprint-clj"
                     :args [:--hang
                            :-i :$FILENAME
                            :-o :/]
                     :use_cache true
                     :to_stdin false
                     :to_temp_file true
                     :ignore_stderr false
                     :from_temp_file true
                     :on_output on-output})]


    {:name "zprint-clj"
     :filetypes ["clojure"]
     :method (. methods :FORMATTING)
     :async true
     : generator}))

(comment (main (require :null-ls)))
