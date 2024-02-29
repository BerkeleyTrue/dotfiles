(module lib.corpus.ftdetect
  {autoload
   {a aniseed.core
    r r}
   require {}
   import-macros []
   require-macros [macros]})

(defn exists? [path]
  (or (= (vf filereadable path) 1)
      (not= (vf glob path) "")))

(comment
  (exists? (.. (vf expand "%:p:h:h:h:h") "/fnl")))

(defn search [path filename]
  (var count 0)
  (var p nil)
  (var folder (vf fnamemodify path ":h"))
  (while (and (not p)
              (not= folder "/")
              (not= folder "~")
              (< count 20))
    (set count (+ count 1))
    (let [_p (.. folder "/" filename)]
      (if (exists? _p)
        (set p _p)
        (set folder (vf fnamemodify folder ":h")))))
  p)

(comment
  (vf glob (vf fnamemodify "fnl" ":p"))
  (search (vf expand "%") ".corpus")
  (search (vf expand "%:p") "fnl"))

(defn get-root [path]
  (let [path (vf fnamemodify ":p:h" path)]
    (or (search path ".corpus")
        (search path ".git"))))

(defn ftdetect [file]
  (if-let [p (search file ".corpus")]
    (do
      (bo! filetype "markdown.corpus")
      true)
    false))

