(module lib.corpus.yadm
  {autoload
   {a aniseed.core
    r r
    utils utils
    {: async
     : await} lib.async
    spawn lib.spawn}
   require {}
   import-macros [[{: defasync} :lib.async-macros]]
   require-macros [macros]})

(def- cmd* :yadm)
(def remote "https://github.com/berkeleytrue/dotfiles.git")

(defn yadm [& args]
  "Run yadm command with args. If job runs successfully,
  yields (true results: string[]), else return (false error:string)."
  (spawn.run* {:command cmd* :args args}))

(comment
  ((async
     (fn []
       (let [(ok? results) (await (yadm "status"))]
         (assert ok? results)
         results)))
   (fn [ok? results]
     (a.println ok? results))))

(defasync get-current-branch []
  (let [(ok? res) (await (yadm :rev-parse :--abbrev-ref :HEAD))]
    (assert ok? res)
    (r.join "\n" res)))

(comment
  ((async
     (fn []
       (let [(ok? results) (await (get-current-branch))]
         (assert ok? results)
         (a.println ok? results)
         results)))
   (fn [ok? results]
     (assert ok? results)
     (a.println results))))
