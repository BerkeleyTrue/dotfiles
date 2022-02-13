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

(defn- check-if-on-whitespace []
  (let [col (- (utils.fn.col ".") 1)]
    (or (= col 0)
        (not
          (r.nil?
            (->
              (utils.fn.getline :.)
              (: :sub col col)
              (: :match :%s)))))))

(defn- enter-mapping [cmp]
  (fn [fallback]
    (print "--Deprecated: Use <C-y> instead--")
    (if
      (= (utils.fn.UltiSnips#CanExpandSnippet) 1) (keys.feed-noremap "<C-R>=UltiSnips#ExpandSnippet()<CR>")
      (and
        (cmp.visible)
        ; make sure something is selected
        (cmp.get_active_entry)) (cmp.confirm {:select false})
      (check-if-on-whitespace) (keys.feed-noremap "<CR>")
      (fallback))))

(defn- tab-mapping [cmp]
  (fn [fallback]
    (print "--Deprecated: Use <C-n> instead--")
    (if
      (cmp.visible) (cmp.select_next_item)
      (= (utils.fn.UltiSnips#CanJumpForwards) 1) (keys.feed "<ESC>:call UltiSnips#JumpForwards()<CR>")
      (check-if-on-whitespace) (keys.feed-noremap :<Tab>)
      (fallback))))

(defn- stab-mapping [cmp]
  (fn [fallback]
    (print "--Deprecated: Use <C-p> instead--")
    (if
      (cmp.visible) (cmp.select_prev_item)
      (= (utils.fn.UltiSnips#CanJumpBackwards) 1) (keys.feed "<ESC>:call UltiSnips#JumpBackwards()<CR>")
      (check-if-on-whitespace) (keys.feed-noremap :<S-Tab>)
      (fallback))))

(defn- create-formatter []
  (when-let [lspkind (lspkind.main)]
    (lspkind.cmp_format
      {:with_text true
       :menu
       {:nvim_lsp "[LSP]"
        :conjure "[conj]"
        :ultisnips "[ulti]"
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

(defn main []
  (when-let [cmp (md.packadd-n-require :nvim-cmp :cmp)]
    (cmp.setup
      {:sources
       [{:name :nvim_lsp}
        {:name :luasnip}
        {:name :conjure :max_item_count 10}
        {:name :path}
        {:name :tmux}
        {:name :treesitter} ; Not sure if this is workig yet
        {:name :nvim_lua}
        {:name :ultisnips}
        {:name :dictionary :keyword_length 2 :max_item_count 5}
        {:name :buffer :keyword_length 5}
        {:name :emoji :insert true :max_item_count 5}]

       :snippet
       {:expand
        (fn [args]
          (utils.fn.UltiSnips#Anon (. args :body))
          (when-let [luasnip (md.prequire :luasnip)]
            (luasnip.lsp_expand args.body)))}

       :mapping
       {:<CR> (cmp.mapping (enter-mapping cmp) [:i :s])
        :<Tab> (cmp.mapping (tab-mapping cmp) [:i :s])
        :<S-Tab> (cmp.mapping (stab-mapping cmp) [:i :s])

        :<C-y>
        (cmp.mapping
          (cmp.mapping.confirm
            {:behavior cmp.ConfirmBehavior.Insert
             :select true})
          [:i :s])

        :<C-Space>
        (cmp.mapping
          {:i (cmp.mapping.complete)
           :c
           (fn command-mode [fallback]
             (if
               (cmp.visible)
               (cmp.confirm {:select true})

               (cmp.complete)))})

        :<C-d> (cmp.mapping.scroll_docs -4)
        :<C-f> (cmp.mapping.scroll_docs 4)
        :<C-e> (cmp.mapping.close)}

       :formatting
       {:format (create-formatter)}

       :experimental
       {:native_menu false
        :ghost_text true}})

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
