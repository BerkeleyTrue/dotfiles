(module plugins.corpus
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (global CorpusDirectories
    {"~/dvlpmnt/node/mr/notes"
     {:autocommit true
      :autoreference true
      :autotitle true
      :base "./"
      :transform "local"}

     "~/docs/notes/corpus"
     {:autocommit true
      :autoreference true
      :autotitle true
      :base "./corpus"
      :transform "local"}

     "~/docs/corpus"
     {:autocommit true
      :autoreference true
      :autotitle true
      :base "./corpus"
      :transform "local"}}))
