(module packer-modules
  {:require {a aniseed.core
             packer plugins.packer}})

(def- main-spec
  [{:name "wbthomason/packer.nvim"
    :opt true}
   {:name "tpope/vim-projectionist"
    :description "jump between related files"}
   {:name "nvim-lua/plenary.nvim"}
   {:name "nvim-lua/telescope.nvim"
    :requires [["nvim-lua/popup.nvim"] ["nvim-lua/plenary.nvim"]]}
   {:name "berkeleytrue/colorbuddy.nvim"}
   {:name "norcalli/nvim-colorizer.lua" :opt true}])


(def- lisp-spec
  [{:name "olical/aniseed" :branch "develop"}
   {:name "olical/conjure" :branch "develop"}
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

(packer.config (a.concat main-spec lisp-spec))
