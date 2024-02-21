(module lib.corpus.git
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    job lib.job
    as lib.async}
   import-macros
   [[{: defasync : acase} :lib.async-macros]]
   require-macros [macros]})

(def- cmd* :git)

(comment (def- cmd* :yadm))

(defn git [& args]
  "Run git command with args. If job runs successfully,
  return (true results: string[]), else return [false error:string]."
  (job.run* {:command cmd* :args args}))

(comment
  ((as.async (fn [] (let [(ok val) (as.await (git "status"))]
                      (a.println "ok: " ok " val: " val))))))

(defasync new? [file]
  (let [(ok file) (as.await (git :ls-files file))]
    (a.println "ok: " ok " file: " file)
    (and ok (r.empty? file))))

(comment
  ((as.async (fn [] (let [(ok _new?) (as.await (new? "kanban.md"))]
                      (a.println :new? _new?)))))
  ((as.async (fn [] (let [(ok val) (as.await (new? "foo.md"))]
                      (a.println :new? val))))))

(defasync dirty? [file]
  (let [(ok res) (as.await (git :diff file))]
    (r.not-empty? res)))

(comment
  ((as.async (fn [] (let [(ok val) (as.await (dirty? (vf expand "%")))]
                      (a.println :ok ok :dirty? val)))))
  ((as.async (fn [] (let [(ok val) (as.await (new? "lazy-lock.json"))]
                      (a.println :dirty? val))))))

(defasync commit [file]
  "commit file, check if file is already in the index, if not add it and commit it."
  (a.println :commiting file)
  (acase (<- (new? file))
    (pure new? (.. "docs: " (if new? :create :update) " " (vim.fn.fnamemodify file ":r") " (corpus)"))
    (<- subject (git :commit :-m subject :-- file))
    (pure commit (a.println "Corpus: file committed: " file commit))
    (catch err (a.println (.. "Corpus: error committing: " err))))) ; nil
