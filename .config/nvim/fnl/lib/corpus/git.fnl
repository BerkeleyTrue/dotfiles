(module lib.corpus.git
  {autoload
   {a aniseed.core
    r r
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
    (assert ok file)
    (r.empty? file)))

(comment
  ((async (fn [] (let [(ok _new?) (await (new? "kanban.md"))]
                      (a.println :new? _new?)))))
  ((async (fn [] (let [(ok val) (await (new? "foo.md"))]
                      (a.println :new? val))))))

(defasync dirty? [file]
  "Check if file is dirty.
  If file is not in the index, return false.
  Otherwise, return true."
  (let [(ok res) (await (git :diff file))]
    ; ok will be false if file is not in the index
    ; which we don't care about
    (and ok (r.not-empty? res))))

(comment
  (vf fnamemodify (vf expand "%") ":r")
  ((async (fn [] (let [(ok val) (await (dirty? (vf expand "%")))]
                      (a.println :ok ok :dirty? val)))))
  ((async (fn [] (let [(ok val) (await (dirty? "lazy-lock.json"))]
                      (a.println :dirty? val)))))
  ((async (fn [] (let [(ok val) (await (dirty? "foo.md"))]
                      (a.println :ok ok :dirty? val))))))


(defasync should-add? [file]
  "Check if file should be added to the index.
  If file is new or dirty, return true. Otherwise, return false."
  (let [(ok _new?) (await (new? file))
        _ (assert ok _new?)
        (ok2 dirty?) (await (dirty? file))
        _ (assert ok2 dirty?)]
    (or _new? dirty?)))

(defasync commit [file]
  "commit file, check if file is already in the index, if not add it and commit it."
  (let [cwd (.. (vf getcwd) "/")
        path (->
               (vf fnamemodify file ":r")
               (string.gsub cwd ""))
        (ok should-add?) (await (should-add? file))]
    (assert ok should-add?)
    (when should-add?
      (acase (<- (git :add file))
        (pure new? (.. "docs: " (if new? :create :update) " " path " (corpus)"))
        (<- subject (git :commit :-m subject :-- file))
        (pure commit (print "Corpus: " file " committed. \n\n" (r.join "\n\t\t" commit)))
        (catch err (a.println "Corpus: error committing: " (if (r.table? err) (r.join "\\n" err) err))))))) ; nil

(comment
  (r.join "\n" ["foo" "bar"])
  ((async (fn [] (let [(ok val) (await (commit (vf expand "%")))]
                   (a.println :ok ok :dirty? val))))))
