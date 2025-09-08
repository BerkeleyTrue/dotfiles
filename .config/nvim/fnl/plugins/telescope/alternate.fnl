(module plugins.telescope.alternate
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    telescope telescope
    alt telescope-alternate}
   require-macros [macros]})



(defn init []
  (command! :TAlt "Telescope telescope-alternate alternate_file" {:desc "Search for alt files"})
  (nnoremap :<leader>fa #(alt.alternate_file) {:silent true}) 

  (g! telescope_alternate 
      {:mappings
       ;; Fennel/Lua
       [{:pattern "^fnl/(.*).fnl"
         :targets
         [{:template "lua/[1].lua"
           :label "Output"}
          {:template "test/fnl/[1]-test.fnl"
           :enable_new false
           :label "Test"}]}

        {:pattern "^lua/(.*).lua"
         :targets
         [{:template "fnl/[1].fnl"
           :label "Source"}]}

        ; Fennel/Lua Tests
        {:pattern "^test/fnl/(.*)-test.fnl"
         :targets
         [{:template "test/lua/[1]-test.lua"
           :label "Output"}
          {:template "fnl/[1].fnl"
           :label "Source"}]}

        {:pattern "^test/lua/(.*).lua"
         :targets
         [{:template "test/fnl/[1].fnl"
           :label "Source"}]}

        ;; Golang
        {:pattern "(.*).templ"
         :targets
         [{:template "[1]_templ.go"
           :label "Output"}]}

        {:pattern "(.*)_templ.go"
         :targets
         [{:template "[1].templ"
           :label "Template"}]}]}))
