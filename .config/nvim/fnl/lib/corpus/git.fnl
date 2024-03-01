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

(defasync new? [file cwd]
  (let [(ok file) (await (git :-C cwd :ls-files file))]
    (assert ok file)
    (r.empty? file)))

(comment
  ((async (fn [] (let [(ok _new?) (await (new? "kanban.md"))]
                      (a.println :new? _new?)))))
  ((async (fn [] (let [(ok val) (await (new? "foo.md"))]
                      (a.println :new? val))))))

(defasync dirty? [file cwd]
  "Check if file is dirty.
  If file is not in the index, return false.
  Otherwise, return true."
  (let [(ok res) (await (git :-C cwd :diff file))]
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
(defasync commit [file cwd]
  "commit file, check if file is already in the index, if not add it and commit it."
  (let [path (->>
               (vf fnamemodify file ":r")
               (r.get-relative-path cwd)
               (r.lsub "^/" ""))
        (ok? is-new?) (await (new? file cwd))
        _ (assert ok? is-new?)
        (ok2 is-dirty?) (await (dirty? file cwd))
        _ (assert ok2 is-dirty?)
        subject (.. "docs: " (if is-new? :create :update) " " path " (corpus)")]
    (when (or is-new? is-dirty?)
      (acase (<- (git :-C cwd :add file))
        (<- nil (git :-C cwd :commit :-m subject :-- file))
        (pure commit (echo [["Corpus" :BerksPurple] [(.. ": " file " committed. \n\n" (r.join "\n\t\t" commit))]]))
        (catch err (n err-writeln (.. "Corpus: error committing: " (if (r.table? err) (r.join "\\n" err) err)))))))) ; nil

(comment
  (r.join "\n" ["foo" "bar"])
  ((async (fn [] (let [(ok val) (await (commit (vf expand "%")))]
                   (a.println :ok ok :dirty? val))))))
