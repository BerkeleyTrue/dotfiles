(module plugins.completion
  {require {: r
            : utils
            a aniseed.core}

   require-macros [macros]})

(defn t [str] (utils.replace_termcodes str true true true))
(comment (t "<CR>"))

(defn feedkeys [str noremap]
  (let [mode (if noremap :n :m)]
    (->
      str
      (t)
      (utils.fn.feedkeys mode))))

(defn feedkeys-noremap [str]
  (feedkeys str true))

(comment
  (feedkeys "i")
  (feedkeys-noremap "i"))

(defn check-if-backspace []
  (let [col (- (utils.fn.col ".") 1)]
    (or (= col 0)
        (not
          (r.nil?
            (->
              (utils.fn.getline :.)
              (: :sub col col)
              (: :match :%s)))))))


(defn enter-mapping [cmp]
  (fn [fallback]
    (if
      (= (utils.fn.UltiSnips#CanExpandSnippet) 1) (feedkeys-noremap "<C-R>=UltiSnips#ExpandSnippet()<CR>")
      (cmp.visible) (feedkeys-noremap " ")
      (check-if-backspace) (feedkeys-noremap "<CR>")
      (fallback))))

(defn tab-mapping [cmp]
  (fn [fallback]
    (if
      (cmp.visible) (cmp.select_next_item)
      (= (utils.fn.UltiSnips#CanJumpForwards) 1) (feedkeys "<ESC>:call UltiSnips#JumpForwards()<CR>")
      (check-if-backspace (feedkeys-noremap :<Tab>))
      (fallback))))

(defn stab-mapping [cmp]
  (fn [fallback]
    (if
      (cmp.visible) (cmp.select_prev_item)
      (= (utils.fn.UltiSnips#CanJumpBackwards) 1) (feedkeys "<ESC>:call UltiSnips#JumpBackwards()<CR>")
      (fallback))))

(defn main []
  (let [(ok res) (pcall utils.ex.packadd :nvim-cmp)]

    ; (if lsp-ok ((: (require :cmp_nvim_lsp) :update_capabilities) (vim.lsp.protocol.make_client_capabilities))
    ;   (print (.. "cannot find cmp-nvim-lsp : " lsp-res)))

    (if
      ok (let [cmp (require :cmp)]
           (cmp.setup
             {:sources
              [{:name :nvim_lsp}
               {:name :ultisnips}
               {:name :buffer}
               {:name :path}
               {:name :emoji
                :insert true}
               {:name :conjure
                :priority 100}]

              :snippet
              {:expand (fn [args] (utils.fn.UltiSnips#Anon (. args :body)))}

              :mapping
              {:<CR> (cmp.mapping (enter-mapping cmp) [:i :s])
               :<Tab> (cmp.mapping (tab-mapping cmp) [:i :s])
               :<S-Tab> (cmp.mapping (stab-mapping cmp) [:i :s])
               :<C-Space> (cmp.mapping.complete)
               :<C-d> (cmp.mapping.scroll_docs -4)
               :<C-f> (cmp.mapping.scroll_docs 4)
               :<C-e> (cmp.mapping.close)}})


           (comment
             {:enabled true
              :debug true
              :min_length 1
              :preselect :enable
              :throttle_time 250
              :source_timeout 500
              :incomplete_delay 250
              :allow_prefix_unmatch false
              :max_abbr_width 100
              :max_kind_width 100
              :max_menu_width 100
              :documentation true
              :source
              {:path {:priority 50}}
              :buffer {:priority 100}
              :calc true
              :spell {:priority 25}
              :vsnip false
              :nvim_lsp {:priority 100}
              :nvim_lua true
              :ultisnips {:priority 110}
              :conjure {:priority 110}
              :nvim_treesitter true}))

      (print "cmp not found in path"))))
