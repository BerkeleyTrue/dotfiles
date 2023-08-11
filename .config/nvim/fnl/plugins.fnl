(module plugins
  {require
   {r r
    lazy lib.lazy}
   require-macros [macros]})

(def- internal-plugins
  [:accents
   :scroll-fix
   :mumber
   :move
   :aniseed])

(def- external-plugins
  ; main utils
  [{:name :berkeleytrue/colorbuddy.nvim}
   {:name :brenoprata10/nvim-highlight-colors :config (make-on-load colorizer)}
   {:name :junegunn/vim-easy-align :init (make-init easy-align)}
   {:name :danilamihailov/beacon.nvim :config (make-on-load beacon)}
   {:name :ntpeters/vim-better-whitespace :config (make-on-load better-whitespace)}
   {:name :mg979/vim-visual-multi :config (make-on-load multi-cursor)}
   {:name :wincent/corpus :description "vim wiki written in lua" :config (make-on-load corpus)}
   {:name :wakatime/vim-wakatime :description "track dev time"}
   {:name :acksld/nvim-revj.lua :description "Inverse of J, break args over lines."}
   {:name :numToStr/comment.nvim :description "Comment engineer written in lua." :config (make-on-load comments)}
   {:name :L3MON4D3/luasnip :description "snippet engine written in lua." :init (make-init luasnip) :config (make-on-load luasnip)}
   {:name :vimsence/vimsence :description "Post activity to Discord \"currently playing\" " :config (make-on-load vimsence)}
   {:name :gennaro-tedesco/nvim-peekup :description "Finally vim registers made easy and fun!"}
   {:name :simrat39/symbols-outline.nvim :description "A tree like view for symbols in Neovim using the Language Server Protocol" :config (make-on-load symbols-outline) :cmd [:SymbolsOutline]}

   ; search
   {:name :nvim-telescope/telescope.nvim :description "Highly extendable fuzzy finder" :dependencies [[:nvim-lua/plenary.nvim]] :config (make-on-load telescope) :lazy false}
   {:name :otavioschwanck/telescope-alternate  :description "jump between related files, like vim-projectionist" :dependencies [[:nvim-telescope/telescope.nvim]]}
   {:name :lukaspietzschmann/telescope-tabs :description "show tabs in telescope" :dependencies [[:nvim-telescope/telescope.nvim]]}

   ; lsp/completion
   {:name :hrsh7th/nvim-cmp :description "completion engine" :config (make-on-load completion)}
   {:name :hrsh7th/cmp-nvim-lsp :description "cmp lsp"}
   {:name :hrsh7th/cmp-buffer :description "cmp buffer"}
   {:name :hrsh7th/cmp-path :description "adds path completion."}
   {:name :hrsh7th/cmp-cmdline :description "Adds command line completion."}
   {:name :saadparwaiz1/cmp_luasnip :description "adds luasnip source"}
   {:name :PaterJason/cmp-conjure :description "Adds conjure completion."}
   {:name :hrsh7th/cmp-emoji :description "Adds emoji completion."}
   {:name :neovim/nvim-lspconfig :description "collection of common configs for lsps" :config (make-on-load lspconfig)}
   {:name :onsails/lspkind-nvim :description "Add pictograms to lsp completion list" :config false}
   {:name :jose-elias-alvarez/null-ls.nvim :description "non lsp lsp sources" :config (make-on-load null-ls)}
   {:name :jose-elias-alvarez/nvim-lsp-ts-utils :description "add a bunch of TS utils"}
   {:name :b0o/schemastore.nvim :description "adds schemastore to jsonls through lspconfig" :dependencies [[:neovim/nvim-lspconfig]] :lazy false}
   {:name :ray-x/cmp-treesitter :description "Adds treesitter completion."}
   {:name :andersevenrud/cmp-tmux :description "Adds tmux completions."}
   {:name :uga-rosa/cmp-dictionary :description "Adds dictionary completions."}
   {:name :zbirenbaum/copilot.lua :description "Add AI overlords to IDE, and I for one welcome them." :event :InsertEnter :config (make-on-load copilot) :cmd [:Copilot]}
   {:name :rcarriga/nvim-notify :description "used by noice, need to set config" :config (make-on-load notify)}
   {:name :folke/noice.nvim :description "Replaces UI for messages, cmdline, and popupmenu" :dependencies [[:MunifTanjim/nui.nvim] [:rcarriga/nvim-notify] [:hrsh7th/nvim-cmp]] :config (make-on-load noice)}
   {:name :SmiteshP/nvim-navic :description "A simple statusline/winbar component that uses LSP to show your current code context." :dependencies [[:neovim/nvim-lspconfig]] :config (make-on-load navic)}


   ; git
   {:name :f-person/git-blame.nvim :description "git blame "}
   {:name :lewis6991/gitsigns.nvim :description "git signs (supports yadm)" :config (make-on-load gitsigns)}
   {:name :sindrets/diffview.nvim :description "git diff view" :dependencies [[:nvim-lua/plenary.nvim]]}

   ; buffers
   {:name :moll/vim-bbye :description "used in Bufdeletehidden"}

   ; motion
   {:name :phaazon/hop.nvim :description "easymotion rewrite in lua with better support for neovim." :config (make-on-load hop)}
   {:name :kevinhwang91/nvim-hlslens :description "incsearch replacement" :config (make-on-load hlslens)}

   ; text obj manipulation
   {:name :tpope/vim-repeat :description "expands . to repeat plugin commands"}
   {:name :wellle/targets.vim :description "adds text objects to give you more target to operate on"}
   {:name :kana/vim-textobj-user :description "create your own text objects easily" :config false}
   {:name :sgur/vim-textobj-parameter :description "argument text parameters" :dependencies [[:kana/vim-textobj-user]] :config false}

   ; tmux integration
   {:name :christoomey/vim-tmux-navigator :description "nav between tmux panes and vim buffs easily"}
   {:name :tmux-plugins/vim-tmux :description "tmux syntax"}

   ; UI
   {:name :nvim-tree/nvim-web-devicons :description "require web devicons through lua"}
   {:name :kshenoy/vim-signature :description "show marks in the gutter"}
   {:name :goolord/alpha-nvim :description "show a dashboard on startup" :config (make-on-load dashboard)}
   {:name :romgrk/barbar.nvim :description "tab bar built in lua" :init (make-init barbar) :config (make-on-load barbar)}
   {:name :nvim-neo-tree/neo-tree.nvim :description "File tree in floats." :dependencies [[:nvim-lua/plenary.nvim] [:nvim-tree/nvim-web-devicons] [:MunifTanjim/nui.nvim]] :config (make-on-load neotree) :cmd [:Neotree] :init (make-init neotree)}
   {:name :nvim-lualine/lualine.nvim :description "lua status line" :dependencies [[:nvim-tree/nvim-web-devicons]] :config (make-on-load lualine)}
   {:name :folke/which-key.nvim :description "Adds popup of possible key bindings."}

   ; parenthesis
   {:name :kylechui/nvim-surround :description "Add surround movements, written in lua." :config (make-on-load surround)}
   {:name :windwp/nvim-autopairs :description "Auto add closing pairs." :config (make-on-load autopairs)}

   ; (lisp)
   ;{:name :olical/aniseed :config (make-on-load aniseed)}
   {:name :olical/conjure :config (make-on-load conjure)}
   {:name :olical/nvim-local-fennel}
   {:name :bakpakin/fennel.vim}

   ; treesitter
   {:name :nvim-treesitter/nvim-treesitter :config (make-on-load tree-sitter) :lazy false}
   {:name :nvim-treesitter/nvim-treesitter-refactor :dependencies [[:nvim-treesitter/nvim-treesitter]]}
   {:name :nvim-treesitter/playground :dependencies [[:nvim-treesitter/nvim-treesitter]]}
   {:name :romgrk/nvim-treesitter-context :dependencies [[:nvim-treesitter/nvim-treesitter]]}
   {:name :andymass/vim-matchup :description "Extends % with treesitter powers." :dependencies [[:nvim-treesitter/nvim-treesitter]]}
   {:name :danymat/neogen :description "Auto doc functions" :dependencies [[:nvim-treesitter/nvim-treesitter]] :config (make-on-load neogen)}
   {:name :JoosepAlviste/nvim-ts-context-commentstring :description "Updates comentstring option using TS"}
   {:name :echasnovski/mini.nvim :description "Lots of mini modules. Mainly used for mini.indent" :config (make-on-load mini)}

   ; general lang syntax plugins
   {:name :gabrielelana/vim-markdown :config (make-on-load markdown)}
   {:name :ellisonleao/glow.nvim :description "Preview markdown code directly in your neovim terminal" :config (make-on-load glow) :cmd [:Glow]}
   {:name :lervag/vimtex :ft :tex :config (make-on-load vimtex)}
   {:name :sirtaj/vim-openscad}
   {:name :purescript-contrib/purescript-vim :description "adds purescript syntax" :ft :purescript}

   ;  
   {:name :mattn/emmet-vim :description "DSL for html" :config (make-on-load emmet)}
   {:name :vuki656/package-info.nvim :description "update package json in nvim" :dependencies [[:MunifTanjim/nui.nvim]] :config (make-on-load package-info)}
   {:name :wavded/vim-stylus :description "stylus lang syntax"}])

(defn main []
  (->>
    internal-plugins
    (r.map #(.. *module-name* "." $1))
    (r.forEach #(run-main $1)))
  (lazy.setup external-plugins))

(comment (main))
