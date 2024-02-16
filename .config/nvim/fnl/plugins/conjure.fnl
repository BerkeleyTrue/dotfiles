(module plugins.conjure
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    extract conjure.extract
    editor conjure.editor
    buffer conjure.buffer
    e conjure.eval}
   require-macros [macros]})

(defn- replace-word []
  "replace a word with the value of that word"
  (when-let [input (extract.word)]
    (let [{: content : range} input
          buf (n win_get_buf 0)
          win (n tabpage_get_win 0)]
      (e.eval-str
        {:code content
         :range range
         :origin :replace-word
         :suppress-hud? true
         :on-result
         (fn [result]
           (buffer.replace-range
             buf
             range
             result)
           (editor.go-to
             win
             (a.get-in range [:start 1])
             (a.inc (a.get-in range [:start 2]))))})
      input)))

(defn init []
  (g! :conjure#client#fennel#aniseed#aniseed_module_prefix "aniseed.")
  (g! :conjure#client#fennel#aniseed#use_metadata true)
  (augroup :PluginConjure
    {:event :BufNewFile
     :pattern :conjure-log-*
     :callback
     (fn conjure-log-autocmd [] (vim.diagnostic.disable 0))}
    {:event :FileType
     :pattern :clojure
     :callback
     (fn conjure-clojure-autocmd []
       (g! :conjure#mapping#doc_word false)
       (g! :conjure#mapping#def_word false))})

  (nnoremap :<leader>ew! replace-word {:silent true :desc "Replace word under the cursor with value using conjure"}))
