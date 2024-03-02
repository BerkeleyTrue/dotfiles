(module plugins
  {require
   {r r
    lazy lib.lazy}
   require-macros [macros]})

(def- internal-plugins
  [:mumber
   :aniseed])

(def- external-plugins
  ; main utils
  [{:name :berkeleytrue/colorbuddy.nvim}
   {:name :danilamihailov/beacon.nvim :config (make-on-load beacon)}
   {:name :wakatime/vim-wakatime :description "track dev time"}
   {:name :acksld/nvim-revj.lua :description "Inverse of J, break args over lines."}
   {:name :gennaro-tedesco/nvim-peekup :description "Finally vim registers made easy and fun! hit double double quote to see a popup of the contents of registers"}
   {:name :simrat39/symbols-outline.nvim :description "A tree like view for symbols in Neovim using the Language Server Protocol" :config (make-on-load symbols-outline) :cmd [:SymbolsOutline]}
   {:name :rmagatti/auto-session :description "auto-session saving for vim" :config (make-on-load auto-session)}

   ; search
   {:name :nvim-telescope/telescope.nvim :description "Highly extendable fuzzy finder" :dependencies [[:nvim-lua/plenary.nvim]] :config (make-on-load telescope) :lazy false}
   {:name :otavioschwanck/telescope-alternate  :description "jump between related files, like vim-projectionist" :dependencies [[:nvim-telescope/telescope.nvim]]}
   {:name :lukaspietzschmann/telescope-tabs :description "show tabs in telescope" :dependencies [[:nvim-telescope/telescope.nvim]]}

   ; lsp/completion
   {:name :hrsh7th/nvim-cmp
    :description "completion popup engine"
    :init (make-init completion)
    :config (make-on-load completion)
    :dependencies [[:onsails/lspkind-nvim]
                   [:saadparwaiz1/cmp_luasnip]
                   [:L3MON4D3/luasnip]]}
   {:name :L3MON4D3/luasnip :description "snippet engine written in lua." :init (make-init luasnip) :config (make-on-load luasnip)}
   {:name :onsails/lspkind-nvim :description "Add pictograms to lsp completion list" :config false}
   {:name :saadparwaiz1/cmp_luasnip :description "adds luasnip source"}
   {:name :hrsh7th/cmp-nvim-lsp :description "cmp lsp"}
   {:name :hrsh7th/cmp-buffer :description "cmp buffer"}
   {:name :hrsh7th/cmp-path :description "adds path completion."}
   {:name :hrsh7th/cmp-cmdline :description "Adds command line completion."}
   {:name :PaterJason/cmp-conjure :description "Adds conjure completion."}
   {:name :hrsh7th/cmp-emoji :description "Adds emoji completion."}
   {:name :neovim/nvim-lspconfig :description "collection of common configs for lsps" :config (make-on-load lspconfig)}
   {:name :nvimdev/guard.nvim :description "Async formatting and linting utility for neovim." :config (make-on-load guard) :dependencies [[:nvimdev/guard-collection]]}
   {:name :jose-elias-alvarez/nvim-lsp-ts-utils :description "add a bunch of TS utils"}
   {:name :b0o/schemastore.nvim :description "adds schemastore to jsonls through lspconfig" :dependencies [[:neovim/nvim-lspconfig]] :lazy false}
   {:name :ray-x/cmp-treesitter :description "Adds treesitter completion."}
   {:name :andersevenrud/cmp-tmux :description "Adds tmux completions."}
   {:name :uga-rosa/cmp-dictionary :description "Adds dictionary completions."}
   {:name :zbirenbaum/copilot.lua :description "Add AI overlords to IDE, and I for one welcome them." :event :InsertEnter :config (make-on-load copilot) :cmd [:Copilot]}
   {:name :rcarriga/nvim-notify :description "used by noice, need to set config" :config (make-on-load notify)}
   {:name :SmiteshP/nvim-navic :description "A simple statusline/winbar component that uses LSP to show your current code context." :dependencies [[:neovim/nvim-lspconfig]] :config (make-on-load navic)}

   ; git
   {:name :f-person/git-blame.nvim :description "git blame "}
   {:name :lewis6991/gitsigns.nvim :description "git signs (supports yadm)" :config (make-on-load gitsigns)}
   {:name :sindrets/diffview.nvim :description "git diff view" :dependencies [[:nvim-lua/plenary.nvim]]}

   ; motion
   {:name :phaazon/hop.nvim :description "easymotion rewrite in lua with better support for neovim." :config (make-on-load hop)}
   {:name :kevinhwang91/nvim-hlslens :description "incsearch replacement" :config (make-on-load hlslens)}
   {:name :wansmer/treesj :description "Plugin for splitting and joining arrays, hashes, and more" :init (make-init treesj) :config (make-on-load treesj) :cmd [:TSJToggle :TSJSplit :TSJJoin]}
   {:name :brenton-leighton/multiple-cursors.nvim
    :description "a better multi cursor written in lua"
    :init (make-init multi-cursor)
    :config (make-on-load multi-cursor)}

   ; text obj manipulation
   {:name :tpope/vim-repeat :description "expands . to repeat plugin commands"}
   {:name :wellle/targets.vim :description "adds text objects to give you more target to operate on"}
   {:name :kana/vim-textobj-user :description "create your own text objects easily" :config false}
   {:name :sgur/vim-textobj-parameter :description "argument text parameters" :dependencies [[:kana/vim-textobj-user]] :config false}

   ; tmux integration
   {:name :christoomey/vim-tmux-navigator :description "nav between tmux panes and vim buffs easily"}
   {:name :tmux-plugins/vim-tmux :description "tmux syntax"}

   ; UI
   {:name :nvim-tree/nvim-web-devicons :description "require web devicons through lua" :config (make-on-load :devicons)}
   {:name :kshenoy/vim-signature :description "show marks in the gutter"}
   {:name :goolord/alpha-nvim :description "show a dashboard on startup" :config (make-on-load dashboard)}
   {:name :romgrk/barbar.nvim :description "tab bar built in lua" :init (make-init barbar) :config (make-on-load barbar)}
   {:name :nvim-neo-tree/neo-tree.nvim 
    :description "File tree in floats." 
    :dependencies [[:nvim-lua/plenary.nvim] 
                   [:nvim-tree/nvim-web-devicons] 
                   [:MunifTanjim/nui.nvim]] 
    :init (make-init neotree)
    :config (make-on-load neotree) 
    :cmd [:Neotree]} 

   {:name :folke/which-key.nvim :description "Adds popup of possible key bindings." :config (make-on-load whichkey)}
   {:name :folke/twilight.nvim :description "Only hightlight current section of buffer"}
   {:name :folke/zen-mode.nvim :description "minimal distraction version of a buffer" :config (make-on-load zenmode)}
   {:name :folke/noice.nvim :description "Replaces UI for messages, cmdline, and history" :dependencies [[:MunifTanjim/nui.nvim] [:hrsh7th/nvim-cmp] [:nvim-telescope/telescope.nvim]] :config (make-on-load noice)}
   {:name :arkav/lualine-lsp-progress :description "add lsp progress to lualine"}
   {:name :nvim-lualine/lualine.nvim
    :description "lua status line"
    :init (make-init lualine)
    :config (make-on-load lualine)
    :dependencies [[:nvim-tree/nvim-web-devicons]
                   [:SmiteshP/nvim-navic]
                   [:arkav/lualine-lsp-progress]
                   [:folke/noice.nvim]]}

   ; parenthesis
   {:name :kylechui/nvim-surround :description "Add surround movements, written in lua." :config (make-on-load surround)}
   {:name :windwp/nvim-autopairs :description "Auto add closing pairs." :config (make-on-load autopairs)}

   ; (lisp)
   ; aniseed provided by nix home-manager
   {:name :olical/conjure :init (make-init conjure)}
   {:name :olical/nvim-local-fennel}
   {:name :bakpakin/fennel.vim}

   ; treesitter
   {:name :nvim-treesitter/nvim-treesitter :config (make-on-load tree-sitter) :lazy false}
   {:name :nvim-treesitter/nvim-treesitter-refactor :dependencies [[:nvim-treesitter/nvim-treesitter]]}
   {:name :nvim-treesitter/playground :dependencies [[:nvim-treesitter/nvim-treesitter]]}
   {:name :romgrk/nvim-treesitter-context :dependencies [[:nvim-treesitter/nvim-treesitter]]}
   {:name :andymass/vim-matchup :description "Extends % with treesitter powers." :dependencies [[:nvim-treesitter/nvim-treesitter]]}
   {:name :danymat/neogen :description "Auto doc functions" :dependencies [[:nvim-treesitter/nvim-treesitter]] :config (make-on-load neogen)}
   {:name :echasnovski/mini.nvim :description "Lots of mini modules. Mainly used for mini.indent" :config (make-on-load mini)}
   {:name :vrischmann/tree-sitter-templ :description "templ tree sitter queries"}

   ; general lang syntax plugins
   {:name :gabrielelana/vim-markdown :config (make-on-load markdown)}
   {:name :lervag/vimtex :ft :tex :config (make-on-load vimtex)}
   {:name :sirtaj/vim-openscad}
   {:name :purescript-contrib/purescript-vim :description "adds purescript syntax" :ft :purescript}
   {:name :kmonad/kmonad-vim :description "adds kmonad config syntax"}

   ;  
   {:name :mattn/emmet-vim :description "DSL for html" :init (make-init emmet) :config (make-on-load emmet)}
   {:name :vuki656/package-info.nvim :description "update package json in nvim" :dependencies [[:MunifTanjim/nui.nvim]] :config (make-on-load package-info)}
   {:name :wavded/vim-stylus :description "stylus lang syntax"}])

(defn main []
  (->>
    internal-plugins
    (r.map #(.. *module-name* "." $1))
    (r.forEach #(run-main $1)))
  (lazy.setup external-plugins))

(comment (main))
