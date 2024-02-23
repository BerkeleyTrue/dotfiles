(module lib.corpus.git
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    {: async
     : await} lib.async
    spawn lib.spawn}
   import-macros
   [[{: defasync : acase} :lib.async-macros]]
   require-macros [macros]})

(def- cmd* :git)

(comment (def- cmd* :yadm))

(defn git [& args]
  "Run git command with args. If job runs successfully,
  yields (true results: string[]), else return (false error:string)."
  (spawn.run* {:command cmd* :args args}))

(comment
  ((async (fn [] (let [(ok val) (await (git "status"))]
                      (a.println "ok: " ok " val: " val))))))

(defasync new? [file]
  (let [(ok file) (await (git :ls-files file))]
    (and ok (r.empty? file))))

(comment
  ((async (fn [] (let [(ok _new?) (await (new? "kanban.md"))]
                      (a.println :new? _new?)))))
  ((async (fn [] (let [(ok val) (await (new? "foo.md"))]
                      (a.println :new? val))))))

(defasync dirty? [file]
  (let [(ok res) (await (git :diff file))]
    (r.not-empty? res)))

(comment
  (vf fnamemodify (vf expand "%") ":r")
  ((async (fn [] (let [(ok val) (await (dirty? (vf expand "%")))]
                      (a.println :ok ok :dirty? val)))))
  ((async (fn [] (let [(ok val) (await (dirty? "lazy-lock.json"))]
                      (a.println :dirty? val)))))
  ((async (fn [] (let [(ok val) (await (dirty? "foo.md"))]
                      (a.println :dirty? val))))))

(defasync add [file]
  "Add file to the index."
  (let [(ok _new?) (await (new? file))]
    (when _new?
      (let [(ok res) (await (git :add file))]
        (assert ok res)))
        ; (a.println "Corpus: file added: " file res))
      ; (a.println "Corpus: file already in index: " file))
    _new?))


(defasync commit [file]
  "commit file, check if file is already in the index, if not add it and commit it."
  (let [cwd (.. (vf getcwd) "/")
        path (->
               (vf fnamemodify file ":r")
               (string.gsub cwd ""))
        (ok dirty) (await (dirty? file))]
    (when (and ok dirty)
      (acase (<- (add file))
        (pure new? (.. "docs: " (if new? :create :update) " " path " (corpus)"))
        (<- subject (git :commit :-m subject :-- file))
        (pure commit (print "Corpus: " file " committed. \n\n" (r.join "\n\t\t" commit)))
        (catch err (a.println "Corpus: error committing: " (if (r.table? err) (r.join "\\n" err) err))))))) ; nil

(comment
  (r.join "\n" ["foo" "bar"])
  ((async (fn [] (let [(ok val) (await (commit (vf expand "%")))]
                   (a.println :ok ok :dirty? val))))))
