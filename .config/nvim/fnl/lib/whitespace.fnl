(module lib.whitespace
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   require-macros [macros]})

(defn trim []
  (let [save-cursor (vf getpos ".")]
    (command keeppatterns "%substitute/\\v\\s+$//eg")
    (vf setpos "." save-cursor)))

(comment (trim))

(defn main []
  (augroup :LibWhiteSpace
    {:event :BufWritePre
     :pattern "*"
     :callback trim}))
