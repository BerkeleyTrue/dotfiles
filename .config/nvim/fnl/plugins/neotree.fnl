(module plugins.neotree
  {require
   {a aniseed.core
    r r
    utils utils
    md utils.module
    hl utils.highlights
    keys utils.keys}
   require-macros [macros]})


(defn main []
  (hl.link! :NeoTreeDirectoryName :Directory)
  (hl.link! :NeoTreeDirectoryIcon :NeoTreeDirectoryName)

  (utils.nnoremap :gef ":NeoTreeFloat<cr>" {:silent true})
  (utils.nnoremap :get ":NeoTreeShow<cr>" {:silent true})

  (when-let [neotree (md.prequire :neo-tree)]
    (let [fs (md.prequire :neo-tree.sources.filesystem)
          cc (md.prequire :neo-tree.sources.common.commands)
          renderer (md.prequire :neo-tree.ui.renderer)]

      (neotree.setup
        {:popup_border_style :rounded
         :enable_git_status true
         :enable_diagnostics true
         :close_floats_on_escape_key false

         :filesystem
         {:filtered_items
          {:hide_dotfiles false
           :hide_gitignore false}

          :follow_current_file false
          :use_libuv_file_watcher false

          :window
          {:position :left
           :width 40

           :mappings
           {:<2-LeftMouse> :open
            :<cr> :open
            :<bs> :nop
            :w :noop
            :qq :close_window

            :<Space>
            (fn toggle-directory [state]
              (let [tree (. state :tree)
                    node (tree:get_node)
                    is-dir (= :directory (. node :type))
                    is-open (when is-dir (: node :is_expanded))]
                (if is-dir
                  (do
                    (fs.toggle_directory state node)
                    (when-not is-open (keys.feed :j)))
                  (cc.close_node state))))

            :<C-h> :open_split
            :<C-v> :open_vsplit

            :<C-t>
            (fn open-tab [state]
              (let [tree (. state :tree)
                    node (tree:get_node)]

                   (when-not (= :directory (. node :type))
                     (let [path (node:get_id)
                           events (md.prequire :neo-tree.events)
                           event-res (events.fire_event
                                       events.FILE_OPEN_REQUESTED
                                       {: state
                                        : path
                                        :open_cmd :tabnew})]

                       (when-not (r.get event-res :handled)
                         (renderer.close_all_floating_windows)
                         (vim.cmd (.. "tabnew " path)))
                       (events.fire_event events.FILE_OPENED path)))))

            :H :close_node
            :L (fn toggle-directory [state]
                 (let [tree (. state :tree)
                       node (tree:get_node)]
                   (when (= :directory (. node :type))
                     (fs.toggle_directory state node)
                     (keys.feed :j))))

            :R :refresh
            :a :add
            :D :delete
            :r :rename
            :/ :nop
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
