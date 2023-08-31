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
      {:mode :text_symbol
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
  [{:name :luasnip}
   {:name :nvim_lsp}
   {:name :conjure :max_item_count 10}
   {:name :path}
   {:name :tmux}
   {:name :treesitter} ; Not sure if this is working yet
   {:name :nvim_lua}
   {:name :dictionary :keyword_length 2 :max_item_count 5}
   {:name :buffer :keyword_length 5}
   {:name :emoji :insert true :max_item_count 5}])

(defn open-on-insert []
  "Close popup if we are on ./,/ or space.
   Open popup if we are on a word. "
  (print "open-on-insert")
  (when-let [cmp (md.prequire :cmp)]
    (let [line (vim.api.nvim_get_current_line)
          cursor (. (vim.api.nvim_win_get_cursor 0) 2)
          current (string.sub line cursor (+ cursor 1))]
      (when (or (= current ".")
                (= current ",")
                (= current " "))
        (print "close")
        (cmp.close))
      (let [before-line (string.sub line 1 (+ cursor 1))
            after-line (string.sub line (+ cursor 1) (- 1))]
        (when (not (string.match before-line "^%s+$"))
          (when (or (= after-line "")
                    (string.match before-line " $")
                    (string.match before-line "%.$"))
            (print "open")
            (cmp.complete)))))))

(comment (open-on-insert))

(defn main []
  (when-let [cmp (md.prequire :cmp)]
    (inoremap :<C-s> "<Cmd>lua require('cmp').complete()<CR>" {:silent true})
    (augroup
      :AutoPopup
      {:event [:TextChangedI :TextChangedP]
       :pattern :*
       :callback open-on-insert})

    (let [luasnip (md.prequire :luasnip)]
      (cmp.setup
        {:sources (get-sources)
         :snippet
         {:expand
          (fn [args]
            (when luasnip
              (luasnip.lsp_expand args.body)))}

         :mapping
         {:<C-y>
          (cmp.mapping
            {:c
             (cmp.mapping.confirm
               {:behavior cmp.ConfirmBehavior.Insert
                :select true})
             :i
             (fn []
               (comment (print :expandable (luasnip.expandable) :selected (not (cmp.get_active_entry))))
               (if (and luasnip (not (cmp.get_active_entry)))
                 (luasnip.expand_auto))
               (cmp.confirm
                 {:behavior cmp.ConfirmBehavior.Insert
                  :select false}))}
            [:i :s :c])

          ; :<C-s>
          ; (cmp.complete
          ;   {:reason cmp.ContextReason.Manual
          ;    :config {:sources (get-sources)}})

          :<C-n>
          (cmp.mapping
            (fn [fallback]
              (if (cmp.visible) (cmp.select_next_item)
                (fallback)))
            [:i :c])

          :<C-p>
          (cmp.mapping
            (fn [fallback]
              (if (cmp.visible) (cmp.select_prev_item)
                (fallback)))
            [:i :c])

          :<C-d> (cmp.mapping.scroll_docs -4)
          :<C-f> (cmp.mapping.scroll_docs 4)
          :<C-e> (cmp.mapping.close)}

         :formatting
         {:format (create-formatter)}

         :experimental
         {:native_menu false
          :ghost_text false}}))

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
         {"*" ["~/.local/share/dict/english"]}
         :debug false
         :document true}))))
