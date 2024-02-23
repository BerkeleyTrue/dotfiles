(module lib.corpus.zettel
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    metadata lib.corpus.metadata}
   require {}
   require-macros [macros]})

(def- zettel-dir "00-zettel")

(defn create []
  "Create a new temp buffer under the zettel directory
  Sets the filename to a temp name.
  Before writing to file, the user is prompted for a title.
  The title is kebab-cased and used as the filename."
  (let [tempid (vf fnameescape (.. "./" zettel-dir "/" "temp-" (math.random) ".md"))]
    (vim.cmd (.. "edit " tempid))
    (metadata.update-file {:force? true :temp? true})))
