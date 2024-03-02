(module lib.yadm
  {autoload
   {a aniseed.core
    r r
    utils utils
    {: async
     : await} lib.async
    spawn lib.spawn}
   require {}
   import-macros [[{: defasync : alet} :lib.async-macros]]
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
  (alet [res (<- (yadm :rev-parse :--abbrev-ref :HEAD))]
    (r.join "\n" res)))

(comment
  ((async
     (fn []
       (alet [results (<- (get-current-branch))]
         (a.println results))))))

(defasync get-log []
  (alet [res (<- (yadm 
                   :log "--pretty=format:[%h] %cs %d **%s** [%cn]" 
                   :--decorate 
                   :-n :10))]
    res))

(defasync print-log []
  (alet [lines (<- (get-log))
         lines (->>
                 lines
                 (r.map #(.. "  - " $))
                 (r.concat ["" ""]))]
     (vim.lsp.util.open_floating_preview lines
       "markdown"
       {:border :rounded
        :pad_left 4
        :pad_right 4
        :relative :buf
        :title "Yadm log"
        :title_pos :left})))

(comment ((print-log)))

(defn main []
  (command! :Ylog #((print-log)) {:desc "Show yadm log in a floating window"}))
