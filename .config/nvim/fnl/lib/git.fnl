(module lib.git
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

(def- cmd* :git)
(def remote "https://github.com/berkeleytrue/dotfiles.git")

(defn git [& args]
  "Run git command with args. If job runs successfully,
  yields (true results: string[]), else return (false error:string)."
  (spawn.run* {:command cmd* :args args}))

(comment
  ((async
     (fn []
       (let [(ok? results) (await (git "status"))]
         (assert ok? results)
         results)))
   (fn [ok? results]
     (a.println ok? results))))

(defasync get-current-branch []
  (let [(ok? res) (await (git :rev-parse :--abbrev-ref :HEAD))]
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

(defasync get-log []
  (let [(ok? res) (await (git :log "--pretty=format:[%h] %cs %d **%s** [%cn]" :--decorate :-n :10))]
    (assert ok? res)
    res))

(defasync print-log []
  (when-let [(ok? lines) (await (get-log))
              lines (->>
                      lines
                      (r.map #(.. "  - " $))
                      (r.concat ["" ""]))]
     (vim.lsp.util.open_floating_preview
       lines
       "markdown"
       {:border :rounded
        :pad_left 4
        :pad_right 4
        :relative :buf
        :title "git log"
        :title_pos :left})))

(comment ((print-log)))

(defn main []
  (command! :Glog #((print-log)) {:desc "Show git log in a floating window"}))
