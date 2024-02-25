(module lib.corpus.zettel
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    metadata lib.corpus.metadata}
   require
   {Input nui.input}
   require-macros [macros]})

(def- zettel-dir "00-zettel")

(def- popup-opts
  {:border {:style :rounded
            :text {:top "[New Zettel]"
                   :top_align :left}}
   :position {:col "50%"
              :row "50%"}
   :relative :editor
   :size 20
   :win_options {:winhighlight "Normal:Normal"}})

(defn- prompt-for-title []
  "Prompts the user for a title using nui.input"
  (fn handle-submit [title]
    (let [filename (vf fnameescape (.. "./" zettel-dir "/" (r.kebab-case title) ".md"))]
      (vim.cmd (.. "edit " filename))
      (metadata.update-file {:force? true})))

  (Input popup-opts {:prompt "Enter title"
                     :on_submit handle-submit}))

(defn is-temp-zet? [filename]
  "Returns true if the filename is a temp zettel"
  (let [tempid (vf fnameescape (.. "./" zettel-dir "/temp-"))]
    (r.starts-with? filename tempid)))

(defn create []
  "Create a new temp buffer under the zettel directory
  Sets the filename to a temp name.
  Before writing to file, the user is prompted for a title.
  The title is kebab-cased and used as the filename."
  (let [tempid (vf fnameescape (.. "./" zettel-dir "/" "temp-" (math.random) ".md"))]
    (vim.cmd (.. "edit " tempid))
    (metadata.update-file {:force? true :temp? true})))

(defn on-pre-write []
  "Before writing to file, prompt the user for a title."
  (a.println :foo)
  (prompt-for-title))
