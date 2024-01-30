(module lib.corpus.ftdetect
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    Path plenary.path}
   require-macros [macros]})

(defn search [path filename]
  (var count 0)
  (var p nil)
  (var folder (: Path :new path))
  (while (and
            (not p)
            (not= (: folder :absolute) "/")
            (< count 20))
    (set count (+ count 1))
    (let [p_ (: folder :joinpath filename)]
      (if (: p_ :exists)
        (set p p_)
        (set folder (: folder :parent)))))
  p)

(defn ftdetect []
  (if-let [p (search (Path:new ".") ".corpus")]
    (do
      (bo! filetype "markdown.corpus")
      true)
    false))
