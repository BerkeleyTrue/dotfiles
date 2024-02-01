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

(defn parse-query [query lang]
  (let [(ok? parsed) (pcall (fn [] (vim.treesitter.query.parse (or lang "markdown") query)))]
    (if ok?
      parsed
      (values nil parsed))))

(defn get-root [bufnr]
  (let [bufnr (or bufnr (n get_current_buf))
        parser (parsers.get_parser bufnr)]
    (: (. (parser:trees) 1) :root)))

(defn get-matches [parsed-query bufnr]
  (let [root (get-root bufnr)
        (start-row _ end-row _) (root:range)]
    (query.iter_prepared_matches parsed-query root bufnr start-row end-row)))

; ## Extractions
(def front-matter-query "(plus_metadata) @toml (minus_metadata) @yaml")

(defn- format-frontmatter [s]
  (->> s
       (r.split "\n") ; split the string into lines
       (r.map vim.trim) ; trim each line
       (r.filter (r.negate r.empty?)) ; remove empty lines
       (r.join "\n"))) ; join the lines back together

(defn extract-frontmatter []
  "Grabs the frontmatter from the current buffer and returns it as a string.
  If there is an error parsing the frontmatter, it prints the error to the console and returns nil.
  If the frontmatter is successfully parsed, it returns the frontmatter as a string."
  (let [bufnr (n get_current_buf)
        get-text (fn [mtch]
                   {:yaml
                    (or
                      (-?>
                        (?. mtch :yaml :node)
                        (vim.treesitter.get_node_text bufnr)
                        (vim.fn.substitute "\\v\\-\\-\\-\\n?" "" "g")
                        (format-frontmatter))
                      "")
                    :toml
                    (or
                      (-?>
                        (?. mtch :toml :node)
                        (vim.treesitter.get_node_text bufnr)
                        (vim.fn.substitute "\\v\\+\\+\\+\\s*\\n?" "" "g")
                        (format-frontmatter))
                      "")})]
    (case-try (parse-query front-matter-query)
      parsed (icollect [mtch (get-matches parsed bufnr)] mtch)
      matches (r.map get-text matches)
      (catch
        (nil err) (do
                    (a.println "Error parsing frontmatter:" err)
                    {})
        _ (do
            (a.println "Error parsing frontmatter:")
            {})))))

(command! :CorpusExtractFrontmatter (fn [] (a.println (extract-frontmatter))))

(def link-ref-def-query "(link_reference_definition
                          (link_label) @link_label
                          (link_destination) @link_destination)")

(comment
  (vf substitute "[label]" "\\v\\[|\\]" "" "g") ; => "label"
  (vf substitute "<./destination>" "\\v\\<|\\>" "" "g")) ; => "./destination"

(defn extract-link-reference-definitions []
  "Extract link reference definitions from the current buffer and return them as a list of maps.
  Each map has a :label and a :destination key. If there is an error parsing the link reference definitions,
  an empty list is returned"
  (let [bufnr (n get_current_buf)
        get-text (fn get-text [mtch]
                   {:label
                    (or (-?>
                          (. mtch :link_label :node)
                          (vim.treesitter.get_node_text bufnr)
                          (vim.fn.substitute "\\v\\[|\\]" "" "g"))
                        "")
                    :destination
                    (or (-?>
                          (. mtch :link_destination :node)
                          (vim.treesitter.get_node_text bufnr)
                          (vim.fn.substitute "\\v\\<|\\>" "" "g"))
                        "")})]

    (case-try (parse-query link-ref-def-query)
      parsed (icollect [mtc (get-matches parsed bufnr)] mtc)
      matches (->> matches (r.map get-text))
      (nil err) (do
                  (a.print "Error parsing link reference definitions:" err)
                  []))))

(command! :CorpusExtractLinkRefDef (fn [] (a.println (extract-link-reference-definitions))))

(def link-shortcut-query "(shortcut_link (link_text) @shortcut_link)")

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
