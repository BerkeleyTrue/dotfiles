(module plugins.completion
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    keys utils.keys
    hl utils.highlights
    lspkind plugins.lspkind}

   require-macros [macros]})

(defn- create-formatter []
  (when-let [lspkind (lspkind.main)]
    (lspkind.cmp_format
      {:with_text true
       :menu
       {:nvim_lsp "[LSP]"
        :conjure "[conj]"
        :luasnip "[snip]"
        :buffer "[buf]"
        :nvim_lua "[lua]"
        :path "[path]"
        :emoji "[😏]"
        :cmdline "[cmd]"
        :buffer "[buff]"
        :tmux "[tmux]"
        :dictionary "[dic]"
        :treesitter "[tree]"}})))

(defn get-sources []
  [{:name :nvim_lsp}
   {:name :luasnip}
   {:name :conjure :max_item_count 10}
   {:name :path}
   {:name :tmux}
   {:name :treesitter} ; Not sure if this is workig yet
   {:name :nvim_lua}
   {:name :dictionary :keyword_length 2 :max_item_count 5}
   {:name :buffer :keyword_length 5}
   {:name :emoji :insert true :max_item_count 5}])

(defn main []
  (when-let [cmp (md.packadd-n-require :nvim-cmp :cmp)]
    (cmp.setup
      {:sources (get-sources)
       :snippet
       {:expand
        (fn [args]
          (when-let [luasnip (md.prequire :luasnip)]
            (luasnip.lsp_expand args.body)))}

       :mapping
       {:<C-y>
        (cmp.mapping
          (cmp.mapping.confirm
            {:behavior cmp.ConfirmBehavior.Insert
             :select true})
          [:i :s])

        :<C-Space>
        (cmp.mapping
          (cmp.mapping.complete
            {:reason cmp.ContextReason.Auto
             :config {:sources (get-sources)}})
          [:i :c])

        :<C-d> (cmp.mapping.scroll_docs -4)
        :<C-f> (cmp.mapping.scroll_docs 4)
        :<C-e> (cmp.mapping.close)}

       :formatting
       {:format (create-formatter)}

       :experimental
       {:native_menu false
        :ghost_text false}})

    ; Adds completion popup to command line!!!!
    (cmp.setup.cmdline ":" {:sources [{:name :cmdline}]})

    (hl.link! :CmpItemAbbr :Comment)
    (hl.link! :CmpItemAbbrDeprecated :Error)
    (hl.link! :CmpItemAbbrMatchFuzz :BerksSubtle)
    (hl.link! :CmpItemKind :Special)
    (hl.link! :CmpItemMenu :NonText)

    (when-let [apcmp (md.prequire :nvim-autopairs.completion.cmp)]
      (cmp.event:on :confirm_cmp (apcmp.on_confirm_done {:map_char {:tex ""}})))

    (when-let [dic (md.prequire :cmp_dictionary)]
      (dic.setup
        {:dic
         {"*" ["/usr/share/dict/english"]}
         :debug false}))))
