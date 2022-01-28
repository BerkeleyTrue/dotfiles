(module plugins.lspconfig
  {:require {a aniseed.core
             : r
             : utils}

   :require-macros [macros]})

(defn get-capabilities []
  (let [cmplsp (require :cmp_nvim_lsp)]
    (cmplsp.update_capabilities (vim.lsp.protocol.make_client_capabilities))))

(defn caramel-configs [lsputil]
  {:default_config
   {:cmd [:caramel-lsp :start]
    :filetypes [:ocaml]
    :root_dir (lsputil.root_pattern ".merlin" "package.json" ".git")
    :settings {}}})

(def lsps
  {:bashls {}
   :caramel_lsp {}
   :clojure_lsp {}
   :cssls {}
   :dockerls {}
   :hls {}
   :html {}
   :jsonls {}
   :rls {}
   :tsserver {:on_attach
              (fn [client]
                (tset client.resolved_capabilities :document_formatting false))}
   :vimls {}
   :yamlls {}})



(defn main []
  (vim.diagnostic.config
    {:virtual_text true
     :signs true
     :float
     {:border :rounded
      :focus false
      :max_width 60
      :scope "cursor"
      :header false}})


  (utils.augroup
    :diagnositic-float
    [{:event [:CursorHold :CursorHoldI]
      :pattern :*
      :cmd "lua vim.diagnostic.open_float()"}])

  (utils.nnoremap :zf ":lua vim.lsp.buf.formatting()<CR>")

  (utils.ex.command_ :Format ":lua vim.lsp.buf.formatting()")

  (let [(ok res) (pcall utils.ex.packadd :nvim-lspconfig)]

    (if (not ok) (print (.. "Could not load nvim-lspconfig: " (tostring res)))
      (let [(ok lspconfig) (pcall require :lspconfig)]
        (if (not ok) (print (.. "require: " lspconfig))
          (let [configs (require :lspconfig.configs)
                lsputil (require :lspconfig.util)]

            (when (not configs.caramel_lsp)
              (set configs.caramel_lsp (caramel-configs lsputil)))

            (->>
              lsps
              (r.to-pairs)
              (r.for-each
                (fn [[lsp config]]
                  (let [conf (. lspconfig lsp)
                        setup (. conf :setup)]
                    (setup (r.merge config {:capabilities (get-capabilities)}))))))))))))
