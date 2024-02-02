(module lib.corpus.metadata
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ts lib.corpus.treesitter}
   require
   {toml toml
    yaml lyaml}
   require-macros [macros]})

(defn decode-toml [str]
  (let [(success data) (pcall toml.decode str)]
    (if success
      data
      (values nil data))))

(defn encode-toml [data]
  (let [(success str) (pcall toml.encode data)]
    (if success
      str
      (values nil str))))

(defn get-frontmatter []
  "get metadata from buffer as a lua table"
  (let [raw (ts.extract-frontmatter)]
    (case raw
      (where {:yaml yamls} (not (r.empty? yamls)))
      (yaml.load yamls)

      (where {:toml toml} (not (r.empty? toml)))
      (case (decode-toml toml)
        (nil err) (a.println :err err)
        data data)

      _ {})))

(comment
  (command! :CorpusGetFrontmatter
            (fn [] (a.println :frontmatter (get-frontmatter)))))

(defn update-frontmatter [data]
  (case (encode-toml data)
    (nil err) (a.println "error updating frontmatter" err)
    str (ts.replace-frontmatter (.. "+++\n" str "\n+++\n"))))

(defn get-title [file]
  "generate a title from filename"
  (vim.fn.fnamemodify file ":t:r"))

(defn update-file [file]
  (let [file (or file (vim.fn.expand "%"))
        title (get-title file)
        metadata (get-frontmatter)]
    (when (not= title (r.get metadata "title"))
      (update-frontmatter (r.assoc metadata "title" title)))))
