(module lib.corpus.search
  {autoload
   {a aniseed.core
    r r
    {: run} lib.spawn}
   require {}
   import-macros []
   require-macros [macros]})

(defn search [input cwd cb]
  (let [terms (->
                input

                (r.lmatch "%S+")
                (r.join "|"))]
    (run {:command :ag
          :cwd cwd
          :args [:--silent
                 :--files-with-matches
                 terms
                 cwd]}
         cb)))
