(module plugins.corpus
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (global CorpusDirectories
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

     "~/docs/notes/captainslog"
     {:autocommit true
      :autoreference true
      :autotitle true
      :transform "local"}

     "~/docs/corpus"
     {:autocommit true
      :autoreference true
      :autotitle true
      :base "./corpus"
      :transform "local"}}))
