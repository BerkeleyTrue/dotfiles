(module lib.yadm
  {autoload
   {a aniseed.core
    r r
    utils utils
    {: async
     : await} lib.async
    spawn lib.spawn
    path lib.path}
   require {}
   import-macros [[{: defasync : alet} :lib.async-macros]]
   require-macros [macros]})

(def- cmd* :yadm)
(def remote "https://github.com/berkeleytrue/dotfiles")

(defn get-file-url [branch filepath line1 line2]
  (let [filepath (.. "/tree/" branch filepath)]
    (if 
      (r.nil? line1) 
      (.. remote filepath)

      (or (= line1 line2) 
          (r.nil? line2)) 
      (.. remote filepath "#L" line1)

      (.. remote filepath "#L" line1 "-L" line2))))

(defn yadm [& args]
  "Run yadm command with args. If job runs successfully,
  yields (true results: string[]), else return (false error:string)."
  (spawn.run* {:command cmd* :args args}))

(comment
  ((async
     (fn []
       (alet [results (<- (yadm "status"))]
         (a.println results))))))

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

(defasync open-file-url [args]
  (alet [filepath (n buf-get-name 0)
         filepath (path.get-relative-path (vf expand "~") filepath)
         branch (<- (get-current-branch))
         url (get-file-url branch filepath args.line1 args.line2)
         res (<- (path.xdg-open* url))]
    (a.println :res url)
    res))

(comment
  (n buf-get-name 0))

(defn main []
  (command! :YadmLog #((print-log)) {:desc "Show yadm log in a floating window"})
  (command! :YadmOpenFile #((open-file-url $)) {:range true :desc "Open file in browser"}))
