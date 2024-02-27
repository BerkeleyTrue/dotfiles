(module plugins.completion
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    keys utils.keys
    hl utils.highlights
    cmp cmp
    lspkind lspkind
    luasnip luasnip}
   require {}
   require-macros [macros]})

(defn- create-formatter []
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
      :treesitter "[tree]"}}))

(defn get-sources []
  [{:name :luasnip}
   {:name :nvim_lsp}
   {:name :conjure :max_item_count 100}
   {:name :path}
   {:name :tmux}
   {:name :treesitter} ; Not sure if this is working yet
   {:name :nvim_lua}
   {:name :dictionary :max_item_count 200}
   {:name :buffer :keyword_length 5}
   {:name :emoji :insert true :max_item_count 50}
   {:name :corpus}])

(defn open-on-insert []
  "Manually open the completion pom when we are in empty space.
  This allows us to open the pom with a virtual keyword length of zero.
  We do this at the end of the line or after some whitespace.
  We still allow cmp to autocomplete for us in most cases."
  (when (not= (n buf_get_option 0 :buftype) :prompt) ; don't open in a prompt (telescope)
    (let [line (n get-current-line)
          [_ col] (n win-get-cursor 0)
          current (string.sub line col (+ col 1))]
      (let [before-cursor (string.sub line 1 (+ col 1)) ; get the content before the cursor
            after-cursor (string.sub line (+ col 1) -1)] ; get the content after the cursor
        (when-not (string.match before-cursor "^%s+$") ; make sure we are not on an empty line (might get remove this? condition)
          (when (or (= after-cursor "") ; If we are at the end of the line
                    (string.match before-cursor "%s+$")) ; or if we are about to start a new word
            (cmp.complete)))))))

(defn init []
  ; Manually trigger completion in insert mode
  (inoremap :<C-s> #(cmp.complete) {:silent true})
  ; not sure what I was thinking here??
  (augroup
    :AutoPopup
    ; after the text was changed in insert mode
    {:event [:TextChangedI
             :TextChangedP]
     :pattern :*
     ; we throttle and defer the callback
     :callback (r.defer-throttle open-on-insert)})

  (hl.link! :CmpItemAbbr :Comment)
  (hl.link! :CmpItemAbbrDeprecated :Error)
  (hl.link! :CmpItemAbbrMatchFuzz :BerksSubtle)
  (hl.link! :CmpItemKind :Special)
  (hl.link! :CmpItemMenu :NonText))

(defn main []
  (cmp.setup
    {:enabled (fn cmp-enabled? []
                (let [buftype (n buf_get_option 0 :buftype)]
                  (not= buftype :prompt)))

     :sources (get-sources)
     :snippet {:expand (fn [args] (luasnip.lsp_expand args.body))}

     :mapping
     {:<C-y>
      (cmp.mapping
        {:c (cmp.mapping.confirm
              {:behavior cmp.ConfirmBehavior.Insert
               :select true})

         :i (fn []
              (comment (print :expandable (luasnip.expandable) :selected (not (cmp.get_active_entry))))
              (if (cmp.get_selected_entry)
                (cmp.confirm
                  {:behavior cmp.ConfirmBehavior.Insert
                   :select false})
                (luasnip.expand_auto)))}
        [:i :s :c])

      :<C-n>
      (cmp.mapping
        (fn [fallback]
          (if (cmp.visible)
            (cmp.select_next_item)
            (fallback)))
        [:i :c])

      :<C-p>
      (cmp.mapping
        (fn [fallback]
          (if (cmp.visible)
            (cmp.select_prev_item)
            (fallback)))
        [:i :c])

      :<C-d> (cmp.mapping.scroll_docs -4)
      :<C-f> (cmp.mapping.scroll_docs 4)
      :<C-e> (cmp.mapping.close)}

     :formatting {:format (create-formatter)}

     :experimental {:native_menu false
                    :ghost_text false}
     :performance {:max_view_entries 20}
     :window
     {:completion (cmp.config.window.bordered)
      :documentation (cmp.config.window.bordered)}})

  ; Adds completion popup to command line!!!!
  (cmp.setup.cmdline ":" {:sources [{:name :cmdline}]})

  (when-let [apcmp (md.prequire :nvim-autopairs.completion.cmp)]
    (cmp.event:on :confirm_cmp (apcmp.on_confirm_done {:map_char {:tex ""}})))

  (when-let [dic (md.prequire :cmp_dictionary)]
    (dic.setup
      {:document {:enable true
                  :command ["wn" "${label}" "-over"]}
        :max_number_items 100
        :paths [(vim.fn.expand "~/.local/share/aspell/english")]})))
