(module packer-modules
  {:require {a aniseed.core
             packer plugins.packer}})

(def- main-spec
  ; main utils
  [{:name "wbthomason/packer.nvim"
    :opt true}
   {:name "tpope/vim-projectionist"
    :description "jump between related files"}
   {:name "nvim-lua/plenary.nvim"}
   {:name "nvim-lua/telescope.nvim"
    :requires [["nvim-lua/popup.nvim"] ["nvim-lua/plenary.nvim"]]}
   {:name "berkeleytrue/colorbuddy.nvim"}
   {:name "norcalli/nvim-colorizer.lua"
    :opt true}
   {:name "junegunn/vim-easy-align"
    :opt true}
   {:name :ashisha/image.vim}
   {:name :danilamihailov/beacon.nvim}
   {:name :ntpeters/vim-better-whitespace}
   {:name :mg979/vim-visual-multi}
   {:name :scrooloose/nerdcommenter}
   {:name :johannesthyssen/vim-signit :desciption "Add signature to a file"}


   ; tmux integration
   {:name :christoomey/vim-tmux-navigator}

   ; git
   {:name :mhinz/vim-signify :desciption "show git diffs in the gutter"}

   ; UI
   {:name :kshenoy/vim-signature :desciption "show marks in the gutter"}
   {:name :mhinz/vim-startify :desciption "show a start up screen"}
   {:name :romgrk/barbar.nvim :desciption "tab bar built in lua"}

   ; (())
   {:name "olical/aniseed" :branch "develop"}
   {:name "~/dvlpmnt/lisp/conjure"}
   {:name "bakpakin/fennel.vim"}
   {:name "eraserhd/parinfer-rust" :run "cargo build --release"}

   {:name "clojure-vim/vim-jack-in"
    :description "Cider style jack in support for Lein, Boot and Clj"
    :requires [["radenling/vim-dispatch-neovim"] ["tpope/vim-dispatch"]]}

   {:name "berkeleytrue/nvim-local-fennel"
    :branch "feat-aniseed-update"
    :description "Execute local Fennel Lisp files in Neovim upon startup "}

   {:name "guns/vim-clojure-static"
    :description "adds static bindings to clojure files"}
   {:name "tpope/vim-sexp-mappings-for-regular-people"
    :description "adds mappings for easier list manipulation"
    :requires [["guns/vim-sexp"]]}])

(packer.config main-spec)
