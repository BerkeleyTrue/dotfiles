(module plugins.conjure
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn init []
  (g!
    :conjure#client#fennel#aniseed#aniseed_module_prefix
    "aniseed.")
  (augroup :Conjure
    {:event :BufNewFile
     :pattern :conjure-log-*
     :callback (fn conjure-log-autocmd []
                 (vim.diagnostic.disable 0))}
    {:event :FileType
     :pattern :clojure
     :callback (fn conjure-clojure-autocmd []
                 (g!
                   :conjure#mapping#doc_word
                   false)
                 (g!
                   :conjure#mapping#def_word
                   false))}))
