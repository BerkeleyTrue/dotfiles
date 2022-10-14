(module packer-modules
  {require
   {a aniseed.core
    anenv plugins.aniseed
    md utils.module
    packer plugins.packer
    utils utils}
   require-macros [macros]})


(def- main-spec
  ; main utils
  [{:name :wbthomason/packer.nvim :opt true}
   {:name :berkeleytrue/colorbuddy.nvim}
   {:name :NvChad/nvim-colorizer.lua :config (make-on-load colorizer)}
   {:name :junegunn/vim-easy-align :opt true :config (make-on-load easy-align)}
   {:name :danilamihailov/beacon.nvim :config (make-on-load beacon)}
   {:name :ntpeters/vim-better-whitespace :config (make-on-load better-whitespace)}
   {:name :mg979/vim-visual-multi :config (make-on-load multi-cursor)}
   {:name :wincent/corpus :description "vim wiki written in lua" :config (make-on-load corpus)}
   {:name :wakatime/vim-wakatime :description "track dev time"}
   {:name :Shelvak/ale :branch :solc_0.8 :description "linting engine" :config (make-on-load ale)}
   {:name :acksld/nvim-revj.lua :description "Inverse of J, break args over lines."}
   {:name :numToStr/comment.nvim :description "Comment engineer written in lua." :config (make-on-load comments)}
   {:name :L3MON4D3/luasnip :description "snippet engine written in lua." :config (make-on-load luasnip)}
   {:name :vimsence/vimsence :description "Post activity to Discord \"currently playing\" " :config (make-on-load vimsence)}
   {:name :gennaro-tedesco/nvim-peekup :description "Finally vim registers made easy and fun!"}
   {:name :simrat39/symbols-outline.nvim :description "A tree like view for symbols in Neovim using the Language Server Protocol" :config (make-on-load symbols-outline) :cmd [:SymbolsOutline]}

   ; search
   {:name :folke/zen-mode.nvim :description "adds zen-mode to current window" :opt true :config (make-on-load zen) :cmd [:ZenMode]}
   {:name :folke/twilight.nvim :description "Dims inactive portions of code" :opt true :config (make-on-load twilight) :cmd [:Twilight]}
   {:name :nvim-lua/telescope.nvim :requires [[:nvim-lua/popup.nvim] [:nvim-lua/plenary.nvim]] :config (make-on-load telescope)}
   {:name :otavioschwanck/telescope-alternate  :description "jump between related files, like vim-projectionist" :requires [[:nvim-lua/telescope.nvim]]}
   {:name :lukaspietzschmann/telescope-tabs :description "show tabs in telescope" :requires [[:nvim-lua/telescope.nvim]]}

   ; lsp/completion
   {:name :hrsh7th/nvim-cmp :description "completion engine" :config (make-on-load completion)}
   {:name :hrsh7th/cmp-nvim-lsp :description "cmp lsp"}
   {:name :hrsh7th/cmp-buffer :description "cmp buffer"}
   {:name :hrsh7th/cmp-path :description "adds path completion."}
   {:name :hrsh7th/cmp-cmdline :description "Adds command line completion."}
   {:name :saadparwaiz1/cmp_luasnip :desciption "adds luasnip source"}
   {:name :PaterJason/cmp-conjure :description "Adds conjure completion."}
   {:name :hrsh7th/cmp-emoji :description "Adds emoji completion."}
   {:name :neovim/nvim-lspconfig :description "collection of common configs for lsps" :config (make-on-load lspconfig)}
   {:name :onsails/lspkind-nvim :desciption "Add pictograms to lsp completion list"}
   {:name :jose-elias-alvarez/null-ls.nvim :description "non lsp lsp sources" :config (make-on-load null-ls)}
   {:name :j-hui/fidget.nvim :description "add lsp spinner" :config (make-on-load fidget)}
   {:name :jose-elias-alvarez/nvim-lsp-ts-utils :description "add a bunch of TS utils"}
   {:name :b0o/schemastore.nvim :desciption "adds schemastore to jsonls through lspconfig" :requires [[:neovim/nvim-lspconfig]]}
   {:name :ray-x/cmp-treesitter :description "Adds treesitter completion."}
   {:name :andersevenrud/cmp-tmux :description "Adds tmux completions."}
   {:name :uga-rosa/cmp-dictionary :description "Adds dictionary completions."}
   {:name :github/copilot.vim :description "Add AI overlords to IDE, and I for one welcome them."}
   {:name :rcarriga/nvim-notify :description "used by noice, need to set config" :config (make-on-load notify)}
   {:name :folke/noice.nvim :description "Replaces UI for messages, cmdline, and popupmenu" :requires [[:MunifTanjim/nui.nvim] [:rcarriga/nvim-notify] [:hrsh7th/nvim-cmp]] :config (make-on-load noice)}

   ; git
   {:name :f-person/git-blame.nvim :description "git blame "}
   {:name :lewis6991/gitsigns.nvim :description "git signs (supports yadm)" :config (make-on-load gitsigns)}
   {:name :sindrets/diffview.nvim :desciption "git diff view" :requires [[:nvim-lua/plenary.nvim]]}

   ; buffers
   {:name :moll/vim-bbye :description "used in Bufdeletehidden"}

   ; motion
   {:name :phaazon/hop.nvim :description "easymotion rewrite in lua with better support for neovim." :config (make-on-load hop)}
   {:name :kevinhwang91/nvim-hlslens :description "incsearch replacement" :config (make-on-load hlslens)}

   ; text obj manipulation
   {:name :tpope/vim-repeat :description "expands . to repeat plugin commands"}
   {:name :wellle/targets.vim :description "adds text objects to give you more target to operate on"}
   {:name :kana/vim-textobj-user :description "create your own text objects easily"}
   {:name :sgur/vim-textobj-parameter :description "argument text parameters"}

   ; tmux integration
   {:name :christoomey/vim-tmux-navigator :description "nav between tmux panes and vim buffs easily"}
   {:name :tmux-plugins/vim-tmux :description "tmux syntax"}

   ; UI
   {:name :kyazdani42/nvim-web-devicons :description "require web devicons through lua"}
   {:name :kshenoy/vim-signature :desciption "show marks in the gutter"}
   {:name :glepnir/dashboard-nvim :desciption "show a dashboard on startup" :config (make-on-load dashboard)}
   {:name :romgrk/barbar.nvim :desciption "tab bar built in lua" :config (make-on-load barbar)}
   {:name :nvim-neo-tree/neo-tree.nvim :description "File tree in floats." :requires [[:nvim-lua/plenary.nvim] [:kyazdani42/nvim-web-devicons] [:MunifTanjim/nui.nvim]] :config (make-on-load neotree)}
   {:name :nvim-lualine/lualine.nvim :description "lua status line" :requires [[:kyazdani42/nvim-web-devicons]] :config (make-on-load lualine)}
   {:name :folke/which-key.nvim :description "Adds popup of possible key bindings."}

   ; parenthesis
   {:name :kylechui/nvim-surround :description "Add surround movements, written in lua." :config (make-on-load surround)}
   {:name :windwp/nvim-autopairs :description "Auto add closing pairs." :config (make-on-load autopairs)}

   ; (lisp)
   {:name :olical/aniseed :config (make-on-load aniseed)}
   {:name :olical/conjure :config (make-on-load conjure)}
   {:name :olical/nvim-local-fennel}
   {:name :bakpakin/fennel.vim}
   {:name :eraserhd/parinfer-rust :run "cargo build --release"}

   ; treesitter
   {:name :nvim-treesitter/nvim-treesitter :config (make-on-load tree-sitter)}
   {:name :nvim-treesitter/nvim-treesitter-refactor :requires [[:nvim-treesitter/nvim-treesitter]]}
   {:name :nvim-treesitter/playground :requires [[:nvim-treesitter/nvim-treesitter]]}
   {:name :romgrk/nvim-treesitter-context :requires [[:nvim-treesitter/nvim-treesitter]]}
   {:name :p00f/nvim-ts-rainbow :description "Adds color to parens." :requires [[:nvim-treesitter/nvim-treesitter]]}
   {:name :andymass/vim-matchup :description "Extends % with treesitter powers." :requires [[:nvim-treesitter/nvim-treesitter]]}
   {:name :danymat/neogen :description "Auto doc functions" :requires [[:nvim-treesitter/nvim-treesitter]] :config (make-on-load neogen)}
   {:name :JoosepAlviste/nvim-ts-context-commentstring :description "Updates comentstring option using TS"}
   {:name :echasnovski/mini.nvim :description "Lots of mini modules. Mainly used for mini.indent" :config (make-on-load mini)}

   ; general lang syntax plugins
   {:name :gabrielelana/vim-markdown :config (make-on-load markdown)}
   {:name :ellisonleao/glow.nvim :description "Preview markdown code directly in your neovim terminal" :config (make-on-load glow) :cmd [:Glow]}
   {:name :glench/vim-jinja2-syntax}
   {:name :hashivim/vim-terraform :ft :terraform :config (make-on-load terraform)}
   {:name :lervag/vimtex :ft :tex :config (make-on-load vimtex)}
   {:name :pearofducks/ansible-vim}
   {:name :sirtaj/vim-openscad}

   ;   
   {:name :mattn/emmet-vim :description "DSL for html" :config (make-on-load emmet)}
   {:name :vuki656/package-info.nvim :description "update package json in nvim" :requires [[:MunifTanjim/nui.nvim]] :config (make-on-load package-info)}
   {:name :wavded/vim-stylus :description "stylus lang syntax"}])

(defn main []
  (packer.config main-spec)
  (command! :SourceModules (viml->lua* source-modules)))

(defn source-modules []
  (anenv.compile-fnl)
  (utils.ex.source (get-lua-filename))
  (packer.config main-spec))
