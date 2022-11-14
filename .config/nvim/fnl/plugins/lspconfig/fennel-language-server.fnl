(module plugins.lspconfig.fennel-language-server
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main [lsputil]
  {:default_config
   {:cmd [:fennel-language-server]
    :filetypes [:fennel]
    :root_dir (lsputil.root_pattern :fnl)
    :single_file_support true

    :settings
    {:fennel
     {:workspace
      {:library (vim.api.nvim_list_runtime_paths)}

      :diagnostics
      {:globals [:vim]}}}}})
