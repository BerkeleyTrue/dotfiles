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
      (values nil (r.join "\n" err))
      (r.join "\n" res))))

(defn commit [file op]
  (let [subject (.. "docs: " op " " (vim.fn.fnamemodify file ":r"))]
    ; in case of new file
    (case-try (git "add" "--" file)
      _ (git "commit" "-m" subject "--" file)
      (catch
        (nil err) (a.println "Error commiting file: " err)))))
