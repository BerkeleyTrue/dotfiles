(module plugins.noice
  {autoload
   {a aniseed.core
    r r
    noice noice
    ts telescope}
   require-macros [macros]})

(def- config
  {:cmdline
   {:view :cmdline_popup
    :opts {:buf_options {:filetype :vim}}
    :icons
    {:/ {:icon " " :hl_group :DiagnosticWarn}
     :? {:icon " " :hl_group :DiagnosticWarn}
     ":" {:icon " "
          :hl_group :DiagnosticInfo
          :firstc false}}}
   :notify {:enabled true
            :view :mini}
   :history {:enabled true}
   :messages {:enabled true
              :view :mini
              :view_error :mini
              :view_warn :mini
              :view_history :popup
              :view_search false}
   :lsp {:hover {:enabled true}
         :progress {:enabled false}
         :signature {:enabled true}
         :override {:vim.lsp.util.convert_input_to_markdown_lines true
                    :vim.lsp.util.stylize_markdown true
                    :cmp.entry.get_documentation true}}
   :throttle (/ 1000 30)
   :views {:mini {:win_options {:winblend 0
                                :winhighlight {:FloatBoarder :NoicePopupBorder}}
                  :border {:style :rounded
                           :padding [0 1]}
                  :timeout 3000
                  :position {:col "50%"
                             :row "95%"}}}


   :routes [{:filter {:event :msg_show ; redirect Inspect to popup
                      :any [{:find "^Treesitter"}
                            {:find "^Extmarks"}
                            {:find "^Syntax"}
                            {:find "^Semantic Tokens"}]}
             :view :popup}

            ; nvim treesitter TSInstallInfo
            {:filter {:event :msg_show
                      :any [{:find "%[.*%] not installed$"}
                            {:find "%[.*%] installed$"}]}
             :view :popup}
            
            ; doesn't seem to work, it may be that that noice 
            ; doesn't hook into yet to catch them.
            {:filter {:event :msg_show
                      :find "^.+Guard.*not executable$"}
             :opts {:skip true}}]

   :presets {:lsp_doc_border true}})

(comment
  (string.find "[Guard] npx not executable" "^.+Guard.*not executable$"))

(defn main []
  (noice.setup config)
  (ts.load_extension :noice))
