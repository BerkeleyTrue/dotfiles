(module plugins.neotree
  {autoload
   {a aniseed.core
    r r

    utils utils
    hl utils.highlights
    keys utils.keys

    neotree neo-tree
    cc neo-tree.sources.common.commands
    fs neo-tree.sources.filesystem.commands
    renderer neo-tree.ui.renderer
    events neo-tree.events}
   require-macros [macros]})

(defn init []
  "initial setup for neotree"
  (nnoremap :geb ":Neotree reveal buffers float<cr>" {:silent true})
  (hl.link! :NeoTreeDirectoryName :Directory)
  (hl.link! :NeoTreeDirectoryIcon :NeoTreeDirectoryName))

(defn handle-new-tab [state]
  "open file in new tab"
  (let [tree (. state :tree)
        node (tree:get_node)]

      (when-not (= :directory (. node :type))
        (let [path (node:get_id)
              event-res (events.fire_event
                          events.FILE_OPEN_REQUESTED
                          {: state
                           : path
                           :open_cmd :tabnew})]

          (when-not (r.get event-res :handled)
            (renderer.close_all_floating_windows)
            (command tabnew path))
          (events.fire_event events.FILE_OPENED path)))))

(defn create-handle-right [toggle]
  "Open node and move into it if it's a directory"
  (fn handle-right [state]
    (let [tree (. state :tree)
          node (tree:get_node)]
      (when (= :directory (. node :type))
        (toggle state)
        (keys.feed :j)))))

(defn create-handle-space [toggle]
  "toggle directory or close node if file is selected"
  (fn handle-space [state]
    (let [tree (. state :tree)
          node (tree:get_node)
          is-dir (= :directory (. node :type))
          is-open (when is-dir (: node :is_expanded))]
      (if is-dir
        (do
          (toggle state)
          (when-not is-open (keys.feed :j)))
        (cc.close_node state)))))

(defn main []
  (neotree.setup
    {:popup_border_style :rounded
     :enable_git_status true
     :enable_diagnostics true
     :close_if_last_window true
     :enable_cursor_hijack true

     :filesystem
     {:filtered_items
      {:hide_dotfiles false
       :hide_gitignored false
       :hide_by_name [:node_modules :.DS_Store]
       :hide_by_pattern [:*.zwc]
       :never_show [:node_modules :.DS_Store]}

      :follow_current_file {:enabled true}
      :use_libuv_file_watcher true

      :window
      {:position :left
       :width 40

       :mappings
       {:<2-LeftMouse> :open
        :<cr> :open
        :<bs> :nop
        :w :noop
        :<esc> :noop
        :qq :close_window

        :<Space> (create-handle-space fs.toggle_node)

        :<C-h> :open_split
        :<C-v> :open_vsplit
        :<C-t> handle-new-tab

        :H :close_node
        :L (create-handle-right fs.toggle_node)

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
       {:<cr> :open
        
        :<Space> (create-handle-space cc.toggle_node)

        :<C-h> :open_split
        :<C-v> :open_vsplit
        :<C-t> handle-new-tab

        :H :close_node
        :L (create-handle-right cc.toggle_node)

        :<bs> :navigate_up
        :<esc> :noop
        :w :noop
        :R :refresh
        :a :add
        :d :buffer_delete
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
        :gg :git_commit_and_push}}}

     :default_component_configs
     {:icon
      {:folder_empty "󱧋"
       :folder_empty_open "󱧋"}
      :git_status
      {:symbols
       {:renamed "󰁕"
        :unstaged  " "}}}

     :document_symbols
     {:kinds
      {:File {:icon "󰈙" :hl "Tag"}
       :Namespace {:icon "󰌗" :hl "Include"}
       :Package {:icon "󰏖" :hl "Label"}
       :Class {:icon "󰌗" :hl "Include"}
       :Property {:icon "󰆧" :hl "@property"}
       :Enum {:icon "󰒻" :hl "@number"}
       :Function {:icon "󰊕" :hl "Function"}
       :String {:icon "󰀬" :hl "String"}
       :Number {:icon "󰎠" :hl "Number"}
       :Array {:icon "󰅪" :hl "Type"}
       :Object {:icon "󰅩" :hl "Type"}
       :Key {:icon "󰌋" :hl ""}
       :Struct {:icon "󰌗" :hl "Type"}
       :Operator {:icon "󰆕" :hl "Operator"}
       :TypeParameter {:icon "󰊄" :hl "Type"}
       :StaticMethod {:icon "󰠄 " :hl "Function"}}}}))
