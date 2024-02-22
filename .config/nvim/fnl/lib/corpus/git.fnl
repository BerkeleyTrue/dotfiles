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
  (a.pr-str {}) ; don't know why this is needed but without it we get a nvim_list_runtime_paths err
  (job.run* {:command cmd* :args args}))

(comment
  ((as.async (fn [] (let [(ok val) (as.await (git "status"))]
                      (a.println "ok: " ok " val: " val))))))

(defasync new? [file]
  (let [(ok file) (as.await (git :ls-files file))]
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
  (vf fnamemodify (vf expand "%") ":r")
  ((as.async (fn [] (let [(ok val) (as.await (dirty? (vf expand "%:r")))]
                      (a.println :ok ok :dirty? val)))))
  ((as.async (fn [] (let [(ok val) (as.await (new? "lazy-lock.json"))]
                      (a.println :dirty? val))))))

(defasync add [file]
  "Add file to the index."
  (let [(ok _new?) (as.await (new? file))]
    (when _new?
      (let [(ok res) (as.await (git :add file))]
        (a.println "Corpus: file added: " file res))
      (a.println "Corpus: file already in index: " file))
    _new?))


(defasync commit [file path]
  "commit file, check if file is already in the index, if not add it and commit it."
  (acase (<- (add file))
    (pure new? (do
                 (a.println :new? new?)
                 (.. "docs: " (if new? :create :update) " " path " (corpus)")))
    (<- subject (git :commit :-m subject :-- file))
    (pure commit (a.println "Corpus: file committed: " file commit))
    (catch err (a.println "Corpus: error committing: " (if (r.table? err) (r.join "\\n" err) err))))) ; nil

(comment
  ((as.async (fn [] (let [(ok val) (as.await (commit (vf expand "%")))]
                      (a.println :ok ok :dirty? val))))))
