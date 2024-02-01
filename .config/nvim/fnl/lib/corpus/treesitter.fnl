(module lib.corpus.treesitter
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ts nvim-treesitter.compat
    parsers nvim-treesitter.parsers}
   require {query nvim-treesitter.query}
   require-macros [macros]})

(def toml-query "(plus_metadata) @frontmatter")
(def yaml-query "(minus_metadata) @frontmatter")
(def link-ref-def-query "(link_reference_definition) @link_reference_definition") ; TODO: capture label/text directly
(def link-shortcut-query "(shortcut_link (link_text) @shortcut_link)")

(defn parse-query [query lang]
  (let [(ok? parsed) (pcall (fn [] (vim.treesitter.query.parse (or lang "markdown") query)))]
    (if ok?
      parsed
      (values nil parsed))))

(defn parse-yaml []
  (parse-query yaml-query))

(defn parse-toml []
  (parse-query toml-query))

(defn get-root [bufnr]
  (let [bufnr (or bufnr (n get_current_buf))
        parser (parsers.get_parser bufnr)]
    (: (. (parser:trees) 1) :root)))

(defn get-matches [parsed-query bufnr]
  (let [root (get-root bufnr)
        (start-row _ end-row _) (root:range)]
    (query.iter_prepared_matches parsed-query root bufnr start-row end-row)))

(defn extract-frontmatter []
  "Grabs the frontmatter from the current buffer and returns it as a string.
  If there is an error parsing the frontmatter, it prints the error to the console and returns nil.
  If the frontmatter is successfully parsed, it returns the frontmatter as a string."
  (let [bufnr (n get_current_buf)
        get-first-match (fn [parsed] ((get-matches parsed bufnr)))
        get-text (fn [mtch] (-> (. mtch :frontmatter :node) (vim.treesitter.get_node_text bufnr)))]
    (case-try (parse-toml)
      parsed (get-first-match parsed)
      mtch (get-text mtch)
      text {:text text :toml true}
      (catch
        _ (case-try (parse-yaml)
            parsed (get-first-match parsed)
            mtch (get-text mtch)
            text {:text text :yaml true}
            (catch
              _ (do (a.println "Nothing found") {})
              (nil err) (do
                          (a.print "Error parsing frontmatter:" err)
                          {})))
        (nil err) (do
                    (a.print "Error parsing frontmatter:" err)
                    {})))))

(command! :CorpusExtractFrontmatter (fn [] (a.println (extract-frontmatter))))

(defn extract-link-reference-definitions []
  (let [bufnr (n get_current_buf)
        get-text (fn get-text [mtch] (-> (. mtch :link_reference_definition :node) (vim.treesitter.get_node_text bufnr)))]

    (case-try (parse-query link-ref-def-query)
      parsed (icollect [mtc (get-matches parsed bufnr)]
                       (do
                         (a.println :match mtc)
                         mtc))
      matches (do
                (a.println :matches matches)
                (->>
                  matches
                  (r.map get-text)))
      (nil err) (do
                  (a.print "Error parsing link reference definitions:" err)
                  []))))

(command! :CorpusExtractLinkRefDef (fn [] (a.println (extract-link-reference-definitions))))

(defn for-each-tree [parsed-query bufnr])

(defn extract-link-shortcuts []
  "Grabs all the link shortcut labels from the current buffer and returns them as a list of strings."
  (let [bufnr (n get_current_buf)
        parser (parsers.get_parser bufnr)
        inlines (. (parser:children) :markdown_inline)
        get-text (fn get-gt [mtch] (-> (. mtch :shortcut_link :node) (vim.treesitter.get_node_text bufnr)))]

    (var out [])
    (case (parse-query link-shortcut-query :markdown_inline)
      (nil err) (do (a.print "Error parsing link shortcuts:" err) [])
      parsed-query
      (do
        (inlines:for_each_tree
          (fn [tree]
            (let [root (tree:root)]
              (icollect [_ node (parsed-query:iter_captures root 0)]
                (table.insert out (vim.treesitter.get_node_text node bufnr))))))
        out))))

(command! :CorpusExtractLinkShortcuts (fn [] (a.println (extract-link-shortcuts))))
