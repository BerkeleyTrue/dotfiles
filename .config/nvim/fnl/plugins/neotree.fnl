(module plugins.neotree
  {require
   {a aniseed.core
    utils utils
    md utils.module
    keys utils.keys}
   require-macros [macros]})


(defn main []
  (utils.hi-link! :NeoTreeDirectoryName :Directory)
  (utils.hi-link! :NeoTreeDirectoryIcon :NeoTreeDirectoryName)
  (utils.nnoremap :gef ":NeoTreeFloat<cr>" {:silent true})
  (when-let [neotree (md.packadd-n-require :neo-tree.nvim :neo-tree)]
    (let [fs (md.prequire :neo-tree.sources.filesystem)
          create-toggle-directory (fn create-toggle-directory [after]
                                    (fn toggle-directory [state]
                                      (let [tree (. state :tree)
                                            node (tree:get_node)]

                                        (when (= :directory (. node :type))
                                          (fs.toggle_directory node)
                                          (when after (after))))))]
      (neotree.setup
        {:popup_border_style :rounded
         :enable_git_status true
         :enable_diagnostics true
         :filesystem

         {:filters
          {:show_hidden false
           :respect_gitignore true}

          :follow_current_file false
          :use_libuv_file_watcher false

          :window
          {:position :left
           :width 40

           :mappings
           {:<2-LeftMouse> :open
            :<cr> :open
            ; TODO: needs to close directory if on file
            :<Space> (create-toggle-directory)
            :<C-h> :open_split
            :<C-v> :open_vsplit

            :H :close_node
            :L (create-toggle-directory (fn [] (keys.feed :j)))

            :R :refresh
            ; :/ :filter_as_you_type
            ; :<c-x> :clear_filter
            :a :add
            :D :delete
            :r :rename
            :C :copy_to_clipboard
            :X :cut_to_clipboard
            :P :paste_from_clipboard}}}

         :buffers
         {:show_unloaded true

          :window
          {:position :left

           :mappings
           {:<2-LeftMouse> :open
            :<cr> :open
            :S :open_split
            :s :open_vsplit
            :<bs> :navigate_up
            :. :set_root
            :R :refresh
            :a :add
            :d :delete
            :r :rename
            :c :copy_to_clipboard
            :x :cut_to_clipboard
            :p :paste_from_clipboard
            :bd :buffer_delete}}}
         :git_status
         {:window
          {:position :float
           :mappings
           {:<2-LeftMouse> :open
            :<cr> :open
            :S :open_split
            :s :open_vsplit
            :C :close_node
            :R :refresh
            :d :delete
            :r :rename
            :c :copy_to_clipboard
            :x :cut_to_clipboard
            :p :paste_from_clipboard
            :A :git_add_all
            :gu :git_unstage_file
            :ga :git_add_file
            :gr :git_revert_file
            :gc :git_commit
            :gp :git_push
            :gg :git_commit_and_push}}}}))))
