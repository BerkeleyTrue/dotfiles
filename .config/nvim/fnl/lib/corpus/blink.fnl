(module lib.corpus.blink
  {autoload
   {a aniseed.core
    r r
    types blink.cmp.types
    ftdetect lib.corpus.ftdetect
    rl lib.corpus.reference-links
    path lib.path
    {: search} lib.corpus.search}
   require {}
   import-macros []
   require-macros [macros]})

(def- keyword-pattern (vim.regex "\\(\\[\\zs[^\\]]*\\ze\\]\\?\\)\\m$"))
(comment 
  (keyword-pattern:match_str "This is a test [exam"))

(var cancellable nil)

(defn new []
  (setmetatable {} {:__index *module*}))

(defn enabled [self]
  (ftdetect.ftdetect (vf expand "%:p")))

(defn get_trigger_characters [self] ["["])

(defn get_completions [self ctx callback]
  (when-let [cwd (ftdetect.search (vf expand "%:p") ".corpus")
             cwd (vf fnamemodify cwd ":h")
             cursor ctx.bounds.start_col
             line_before_cursor (string.sub ctx.line 1 cursor)
             (kw_start _) (keyword-pattern:match_str line_before_cursor)

             input (if kw_start 
                     (string.sub line_before_cursor (+ kw_start 1)) 
                     "")]

    (when (r.not-empty? input)
      (when cancellable
        (cancellable))

      (fn handle-results [ok? results]
        (when ok?
          (let [items (->>
                        results
                        (r.map #{:label (vf fnamemodify $ ":t:r")
                                 :kind types.CompletionItemKind.Reference
                                 :data {:file $}}))]
            (callback {:items items
                       :is_incomplete_backward false
                       :is_incomplete_forward false}))))

      (set cancellable (search input cwd handle-results)))

    (callback {:items []
               :is_incomplete_forward true
               :is_incomplete_backward false})))

(defn resolve [self item callback]
  "Resolve the completion item. This occurs write before displaying the item to the user."
  (set item.documentation "# No documentation available")
  (when-let [file item.data.file
             root (ftdetect.get-corpus-root file)
             file (path.get-relative-path root file)
             content (vf readfile file "" 10)
             document (.. (r.join "\n" content) "\n"
                          "---"
                          "\n\n"
                          "*Corpus*: " file)]

    (set item.documentation document))
  (callback item))

(defn execute [self ctx item callback default-exec]
  "this occurs after a user selects a completion item"
  (default-exec)
  ; give treesitter a chance to catch up with changes
  (vim.schedule rl.update-file)
  (callback))
