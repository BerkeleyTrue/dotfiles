(module lib.corpus.ftdetect
  {autoload
   {a aniseed.core
    r r}
   require {}
   import-macros []
   require-macros [macros]})

(defn search [path filename]
  (var count 0)
  (var p nil)
  (var folder (vf fnamemodify path ":p:h"))
  (while (and
            (not p)
            (not= folder "/")
            (< count 20))
    (set count (+ count 1))
    (let [_p (.. folder "/" filename)]
      (if (= (vf filereadable _p) 1)
        (set p _p)
        (set folder (vf fnamemodify folder ":h")))))
  p)

(defn ftdetect []
  (if-let [p (search (vf expand "%") ".corpus")]
    (do
      (bo! filetype "markdown.corpus")
      true)
    false))

(comment
  (search (vf expand "%") ".corpus")
  (search (vf expand "%") "accents.fnl")

  (vf filereadable (..
                     (vf fnamemodify
                         (vf expand "%:p:h")
                         ":h")
                     "/accents.fnl")))

