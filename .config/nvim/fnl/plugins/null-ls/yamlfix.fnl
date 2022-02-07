(module plugins.null-ls.yamlfix
  {require
   {a aniseed.core
    md utils.module}})

(def name "yamlfix")

(defn on-output [params done]
  (let [output (. params :output)
        err (. params :err)]

    (if err
      (do
        (vim.notify (.. name " could not format"))
        (done))
      (done (when output [{:text output}])))))

(defn main [null-ls]
  (let [methods (. null-ls :methods)
        helpers (md.prequire :null-ls.helpers)
        generator (helpers.generator_factory
                    {:command name
                     :args [:$FILENAME]
                     :use_cache true
                     :to_stdin false
                     :to_temp_file true
                     :ignore_stderr true
                     :from_temp_file true
                     :on_output on-output})]

    {:name name
     :filetypes ["yaml"]
     :method (. methods :FORMATTING)
     :async true
     : generator}))

(comment (main (require :null-ls)))
