(module plugins.aniseed
  {require
   {a aniseed.core
    anenv aniseed.env
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn aniseed-compile []
  (print :recompiling)
  (anenv.init {:force true}))

(defn main []
  (command! :AniseedCompile aniseed-compile)
  (let [fnl-dir (.. (vim.fn.stdpath :config) "/fnl/*.fnl")]

    (augroup
      :AniseedMaps
      {:event :BufReadPost
       :pattern fnl-dir
       :callback (fn add-aniseed-compile []
                   (nnoremap :<leader>ac aniseed-compile {:silent true :buffer true}))})))
