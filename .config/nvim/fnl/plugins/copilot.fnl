(module plugins.copilot
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

; load through packer
(defn main []
  (vim.schedule
    (fn []
      (when-let [copilot-server (md.prequire :copilot)]
        (copilot-server.setup
          {:cmp {:enabled false}
           :plugin_manager_path (.. (vim.fn.stdpath :config) "/pack/packer")})
        (when-let [client (md.prequire :copilot-client)]
          (client.setup {})
          (utils.inoremap :<C-c> (utils.cviml->lua :copilot-client :suggest) {:silent true}))))))
