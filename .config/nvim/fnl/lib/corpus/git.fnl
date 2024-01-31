(module lib.corpus.git
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    Job plenary.job}
   require-macros [macros]})

(defn git [...]
  (let [args [...]
        j (Job:new {:command "git" :args args})
        _  (j:sync)
        code j.code
        res (j:result)
        err (j:stderr_result)]
    (if (not= code 0)
      (values nil (or (r.join "\n" err) ""))
      (r.join "\n" res))))

(defn commit [file op]
  (let [subject (.. "docs: " op " " (vim.fn.fnamemodify file ":r"))]
    (case (git "diff" "--quiet" file)
      (nil err) (case-try (git "add" "--" file)
                  _ (git "commit" "-m" subject "--" file)
                  _ (print "Corpus: file committed")
                  (catch
                    (nil err) (a.println "Error committing file: " err)))
      _ nil)))
