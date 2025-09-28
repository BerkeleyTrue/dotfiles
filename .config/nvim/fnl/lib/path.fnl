(module lib.path
  {autoload
   {a aniseed.core
    r r
    {: run
     : run*} lib.spawn
    as lib.async}
   require {}
   import-macros []
   require-macros [macros]})

(defn get-relative-path [sub-folder-path file]
  "Get the relative path of a file from a sub-folder-path."
  (let [full-path (vf fnamemodify file ":p")
        ; normalize the sub-folder-path to always end with a /
        sub-folder-path (if (= (string.sub sub-folder-path -1) "/")
                          sub-folder-path
                          (.. sub-folder-path "/"))]

    (pick-values 1 (string.gsub full-path (.. "^" sub-folder-path) ""))))

(comment
  (get-relative-path (vf fnamemodify "fnl" ":p") (vf expand "%"))
  (get-relative-path (string.sub (vf fnamemodify "fnl" ":p") 1 -2) (vf expand "%"))
  (get-relative-path "" "src/r/path/init.lua"))

(defn xdg-open [path cb]
  "Open a path in the default application."
  (run {:command :xdg-open :args [path]} cb))

(defn xdg-open* [path]
  "Open a path in the default application."
  (run* {:command :xdg-open :args [path]}))
