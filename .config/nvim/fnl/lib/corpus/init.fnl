(module lib.corpus
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ftdetect lib.corpus.ftdetect}
   require-macros [macros]})

(defn main []
  (augroup :Corpus
    {:event [:BufNewFile :BufRead]
     :pattern :*.md
     :callback
     (fn []
       (ftdetect.ftdetect))}))
