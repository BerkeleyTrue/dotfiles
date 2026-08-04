(module lib.corpus.search
  {autoload
   {a aniseed.core
    r r
    {: run} lib.spawn}
   require {}
   import-macros []
   require-macros [macros]})

(defn search [input cwd cb]
  (let [terms (-> input
                  (r.lmatch "%S+")
                  (r.join "|"))]
    (run {:command :rg
          :cwd cwd
          :args [:--files-with-matches
                 :--no-messages
                 terms
                 cwd]}
         cb)))
