(module r.path
  {autoload
   {a aniseed.core
    r r}
   require {}
   import-macros []
   require-macros [macros]})

(defn get-relative-path [sub-folder-path file]
  (let [full-path (vf fnamemodify file ":p")]
    (pick-values 1 (string.gsub full-path (.. "^" sub-folder-path) ""))))

(comment
  (string.gsub "/home/foo/bar/baz.fnl" "^/home/foo/bar/" "")
  (get-relative-path (vf fnamemodify "fnl" ":p") (vf expand "%"))
  (get-relative-path "" "src/r/path/init.lua"))

