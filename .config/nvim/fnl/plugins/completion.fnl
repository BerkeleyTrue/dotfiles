(module plugins.completion
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    keys utils.keys
    hl utils.highlights
    cmp blink.cmp
    devicons nvim-web-devicons
    lspkind lspkind
    luasnip luasnip}
   require {}
   require-macros [macros]})

; TODO: create corpus source

(defn open-on-insert []
  "Manually open the completion pom when we are in empty space.
  This allows us to open the pom with a virtual keyword length of zero.
  We do this at the end of the line or after some whitespace.
  We still allow cmp to autocomplete for us in most cases."
  (when-not (= (n buf_get_option 0 :buftype) :prompt) ; don't open in a prompt (telescope)
      (let [line (n get-current-line)
            [_ col] (n win-get-cursor 0)
            current (string.sub line col (+ col 1))
            before-cursor (string.sub line 1 (+ col 1)) ; get the content before the cursor
            after-cursor (string.sub line (+ col 1) -1)] ; get the content after the cursor
        (when-not (string.match before-cursor "^%s+$") ; make sure we are not on an empty line
          (when (or (= after-cursor "") ; If we are at the end of the line
                    (string.match before-cursor "%s+$")) ; or if we are about to start a new word
            (cmp.show))))))

(defn init []
  ; Manually trigger completion in insert mode
  (let [debounced (r.debounce 400 open-on-insert)]
    ; not sure what I was thinking here??
    (augroup
      :AutoPopup
      ; after the text was changed in insert mode
      {:event [:TextChangedI
               :TextChangedP]
       :pattern :*
       ; we throttle and defer the callback
       :callback debounced.f}))

  (hl.link! :BlinkCmpDeprecated :Error)
  (hl.link! :BlinkCmpKind :Special)
  (hl.link! :BlinkCmpMenu :NonText)
  (hl.link! :BlinkCmpMenuBorder :FloatBorder)
  (hl.link! :BlinkCmpSource :Special)
  (hl.link! :BlinkCmpLabelDescription :Comment)
  (hl.link! :BlinkCmpMenuSelection :BerksTeal)
  (hl.link! :BlinkCmpScrollBarThumb :BerksTealBg))


(defn devicon-kind-text [ctx]
  (let [icon ctx.kind_icon
        label ctx.label
        kind ctx.kind
        gap ctx.icon_gap
        source-name ctx.source_name
        sym-name (if (vim.tbl_contains [:spell :cmdline :markdown :Dict] source-name) 
                   source-name 
                   kind)
        icon (if 
               (vim.tbl_contains [:Path] ctx.source_name) (let [(dev-icon) (devicons.get_icon label)]
                                                            (or dev-icon icon))
               (lspkind.symbolic sym-name {:mode :symbol}))]
    (.. icon gap)))

(defn devicon-kind-highlight [ctx]
  (let [hl ctx.kind_hl
        source-name ctx.source_name
        label ctx.label
        hl (if (vim.tbl_contains [:Path] source-name)
             (let [(dev-icon dev-hl) (devicons.get_icon label)]
               (if dev-icon dev-hl hl)))]
    hl))

(defn main []
  (cmp.setup 
    {:completion {:keyword {:range :full}
                  :menu {:draw {:components {:kind_icon {:text devicon-kind-text
                                                         :highlight devicon-kind-highlight}
                                             :kind {:highlight devicon-kind-highlight}}
                                :columns [{1 :label 2 :label_description :gap 1} [:kind_icon] {1 :source_name :gap 1}]}
                         :border :rounded}
                  :documentation {:auto_show true
                                  :auto_show_delay_ms 300
                                  :update_delay_ms 50
                                  :window {:border :rounded}}
                  :trigger {:show_on_backspace true}}

     :signature {:enabled true
                 :window {:border :rounded}}
     :cmdline {:enabled true
               :completion {:menu {:auto_show true}}}

     ; TODO: move to vim.snippet
     :snippets {:preset :luasnip}
     :sources {:default [:lsp :path :buffer :snippets :emoji :dictionary :thesaurus :cmp-conjure]

               :providers {:lsp {:score_offset 100}
                           :cmp-conjure {:name :cmp-conjure
                                         :module :blink.compat.source
                                         :score_offset 100
                                         :async true
                                         :opts {:cmp_name :cnj}}
                           :buffer {:name :buf}
                           :emoji {:name :emj
                                   :module :blink-emoji
                                   :score_offset 10}

                           :dictionary {:name :wrds
                                        :module :blink-cmp-words.dictionary
                                        :max_items 10
                                        :score_offset (- 100)}
                           :thesaurus {:name :thsr
                                       :module :blink-cmp-words.thesaurus
                                       :max_items 5
                                       :score_offset (- 200)}}}}))
  ; Leaving this here for now, might want it later if issue exists with blink and autopairs
  ; ; On confirm, setup auto pairs
  ; (cmp.event:on :confirm_cmp (apcmp.on_confirm_done {:map_char {:tex ""}}))
  ;
  ; ; https://github.com/andymass/vim-matchup/pull/382
  ; ; https://github.com/hrsh7th/nvim-cmp/issues/1269))
  ; ; https://github.com/hrsh7th/nvim-cmp/issues/1940)
  ; ; thanks to @perrin4869
  ; (cmp.event:on :menu_opened (fn [] (b! matchup_matchparen_enabled false)))
  ; (cmp.event:on :menu_closed (fn [] (b! matchup_matchparen_enabled true))))
