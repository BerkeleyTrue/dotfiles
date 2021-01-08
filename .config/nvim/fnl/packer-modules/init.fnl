(module packer-modules
  {:require {a aniseed.core
             packer plugins.packer}})

(def- main-spec
  ; main utils
  [{:name :wbthomason/packer.nvim
    :opt true}
   {:name :tpope/vim-projectionist
    :description "jump between related files"}
   {:name :nvim-lua/plenary.nvim}
   {:name :nvim-lua/telescope.nvim
    :requires [[:nvim-lua/popup.nvim] [:nvim-lua/plenary.nvim]]}
   {:name :berkeleytrue/colorbuddy.nvim}
   {:name :norcalli/nvim-colorizer.lua
    :opt true}
   {:name :junegunn/vim-easy-align
    :opt true}
   {:name :ashisha/image.vim}
   {:name :danilamihailov/beacon.nvim}
   {:name :ntpeters/vim-better-whitespace}
   {:name :mg979/vim-visual-multi}
   {:name :scrooloose/nerdcommenter}
   {:name :johannesthyssen/vim-signit :description "Add signature to a file"}
   {:name :wincent/corpus :description "vim wiki written in lua"}
   {:name :wakatime/vim-wakatime :description "track dev time"}
   {:name :berkeleyTrue/berkeleys-snippet-emporium :description "my personal snippets"}
   {:name :sirVer/ultisnips :description "snippet engine"}
   {:name :w0rp/ale :description "linting engine"}

   ; search
   {:name :junegunn/fzf.vim :requires [[:junegunn/fzf]]}

   ; lsp/completion
   {:name :nvim-lua/completion-nvim :desciption "add lua based completion tools"}
   {:name :steelsojka/completion-buffers :description "adds buffer to completion list"}
   {:name :neovim/nvim-lspconfig :description "collection of common configs for lsps"}
   {:name :nvim-treesitter/completion-treesitter :description "add treesitter to completion"}
   {:name :albertoCaroM/completion-tmux :description "add tmux pane info to completion"}

   ; git
   {:name :tpope/vim-fugitive :description "git integrations (lacking yadm support)"}
   {:name :tpope/vim-rhubarb :description "github integrations (lacing yadm support)"}

   ; buffers
   {:name :vim-scripts/BufOnly.vim :description ":BufOnly removes all the buffers except the current one"}
   {:name :moll/vim-bbye :description "used in Bufdeletehidden"}

   ; motion
   {:name :tmhedberg/matchit :description "expands % to match more then single chars"}
   {:name :tpope/vim-unimpaired :description "expands [] to command ex commands"}
   {:name :matze/vim-move :description "move visual selections up, down, left, or right easily"}
   {:name :easymotion/vim-easymotion :description "move with ease using highlighted visual guides"}
   {:name :haya14busa/incsearch.vim :description "inc highlight searchs live" :opt true}
   {:name :haya14busa/incsearch-fuzzy.vim :description "adds fuzzy search to incsearch" :opt true}
   {:name :haya14busa/incsearch-easymotion.vim :description "integrate easymotion with incsearch" :opt true}
   {:name :mileszs/ack.vim :description "use terminal searcher (Ag)"}

   ; text obj manipulation
   {:name :tpope/vim-repeat :description "expands . to repeat plugin commands"}
   {:name :wellle/targets.vim :description "adds text objects to give you more target to operate on"}
   {:name :kana/vim-textobj-user :description "create your own text objects easily"}

   ; tmux integration
   {:name :christoomey/vim-tmux-navigator :description "nav between tmux panes and vim buffs easily"}
   {:name :tmux-plugins/vim-tmux :description "tmux syntax"}
   {:name :tmux-plugins/vim-tmux-focus-events :description "restore focus events when enter/leave vim bufs in tmux panes"}
   {:name :wellle/tmux-complete.vim :desciption "inject words in neighboring tmux panes into the completion popup"}

   ; git
   {:name :mhinz/vim-signify :desciption "show git diffs in the gutter"}

   ; UI
   {:name :kshenoy/vim-signature :desciption "show marks in the gutter"}
   {:name :mhinz/vim-startify :desciption "show a start up screen"}
   {:name :romgrk/barbar.nvim :desciption "tab bar built in lua"}
   {:name :shougo/defx.nvim :description "file explorer"}
   {:name :kristijanhusak/defx-git :description "add git marks for defx"}
   {:name :kristijanhusak/defx-icons :description "add dev icons for defx"}

    ; parenthesis
   {:name :kien/rainbow_parentheses.vim :description "make parens rainbow, bruh"}
   {:name :kyazdani42/nvim-web-devicons :description "require web devicons through lua"}
   {:name :raimondi/delimitMate :description "automatic closing of quotes, parenthesis, brackets"}
   {:name :machakann/vim-sandwich :description "adds surround movements"}
   {:name :valloric/MatchTagAlways :description "highlights the matching tag in html"}

   ; (lisp)
   {:name "~/dvlpmnt/lisp/aniseed"}
   {:name "~/dvlpmnt/lisp/conjure"}
   {:name :bakpakin/fennel.vim}
   {:name :eraserhd/parinfer-rust :run "cargo build --release"}
   {:name :clojure-vim/vim-jack-in
    :description "Cider style jack in support for Lein, Boot and Clj"
    :requires [[:radenling/vim-dispatch-neovim] [:tpope/vim-dispatch]]}
   {:name :berkeleytrue/nvim-local-fennel
    :branch "feat-aniseed-update"
    :description "Execute local Fennel Lisp files in Neovim upon startup "}
   {:name :guns/vim-clojure-static
    :description "adds static bindings to clojure files"}
   {:name :tpope/vim-sexp-mappings-for-regular-people
    :description "adds mappings for easier list manipulation"
    :requires [[:guns/vim-sexp]]}

   ; treesitter
   {:name :nvim-treesitter/nvim-treesitter}
   {:name :nvim-treesitter/nvim-treesitter-refactor :requires [[:nvim-treesitter/nvim-treesitter]]}
   {:name :nvim-treesitter/playground :requires [[:nvim-treesitter/nvim-treesitter]]}
   {:name :romgrk/nvim-treesitter-context :requires [[:nvim-treesitter/nvim-treesitter]]}

   ; general lang syntax plugins
   {:name :plasticboy/vim-markdown :requires [[:godlygeek/tabular]]}
   {:name :chr4/nginx.vim}
   {:name :chrisbra/csv.vim}
   {:name :digitaltoad/vim-jade}
   {:name :glench/vim-jinja2-syntax}
   {:name :hashivim/vim-terraform}
   {:name :lervag/vimtex}
   {:name :othree/xml.vim}
   {:name :pearofducks/ansible-vim}
   {:name :potatoesmaster/i3-vim-syntax}
   {:name :rust-lang/rust.vim}
   {:name :sirtaj/vim-openscad}
   {:name :tbastos/vim-lua}
   {:name :purescript-contrib/purescript-vim}

   ;   
   {:name :elzr/vim-json :description ""}
   {:name :hhsnopek/vim-sugarss :description ""}
   {:name :leafgarland/typescript-vim :description ""}
   {:name :mattn/emmet-vim :description ""}
   {:name :meain/vim-package-info :description "" :run "npm install"}
   {:name :moll/vim-node :description ""}
   {:name :posva/vim-vue :description ""}
   {:name :wavded/vim-stylus :description ""}])

(packer.config main-spec)
