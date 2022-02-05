(module plugins.completion
  {require
   {r r
    md utils.module
    utils utils
    keys utils.keys
    a aniseed.core}

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
    (if
      (cmp.visible) (cmp.select_next_item)
      (= (utils.fn.UltiSnips#CanJumpForwards) 1) (keys.feed "<ESC>:call UltiSnips#JumpForwards()<CR>")
      (check-if-on-whitespace) (keys.feed-noremap :<Tab>)
      (fallback))))

(defn- stab-mapping [cmp]
  (fn [fallback]
    (if
      (cmp.visible) (cmp.select_prev_item)
      (= (utils.fn.UltiSnips#CanJumpBackwards) 1) (keys.feed "<ESC>:call UltiSnips#JumpBackwards()<CR>")
      (check-if-on-whitespace) (keys.feed-noremap :<S-Tab>)
      (fallback))))

(defn main []
  (when-let [cmp (md.packadd-n-require :nvim-cmp :cmp)]
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
    (when-let [apcmp (md.prequire :nvim-autopairs.completion.cmp)]
      (cmp.event:on :confirm_cmp (apcmp.on_confirm_done {:map_char {:tex ""}})))))
