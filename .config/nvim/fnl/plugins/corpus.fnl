(module plugins.corpus
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn init [])

(defn main []
  (global CorpusDirectories
    (->>
      (vim.fn.glob (.. (vim.fn.expand "~/docs/corpus") "/*") 0 1)
      (r.filter (fn [x] (= 1 (vim.fn.isdirectory x))))
      (r.map (fn [x] [x {:autocommit false ; creates a new git repo instead of using root repo
                         :autoreference true
                         :autotitle true
                         :base ""
                         :transform "web"}]))
      (r.from-pairs)
      (r.merge
        {"~/dvlpmnt/madison-reed/notes"
         {:autocommit true
          :autoreference true
          :autotitle true
          :transform "local"}

         "~/docs/notes/corpus"
         {:autocommit true
          :autoreference true
          :autotitle true
          :transform "local"}

         ; daily notes
         "~/docs/notes/captainslog"
         {:autocommit true
          :autoreference true
          :autotitle true
          :transform "web"}

         ; 2nd brain
         "~/docs/corpus"
         {:autocommit true
          :autoreference true
          :autotitle true
          :base ""
          :transform "web"}})))
  (when-let [corpus (md.prequire "corpus")]
    (corpus.setup)))
