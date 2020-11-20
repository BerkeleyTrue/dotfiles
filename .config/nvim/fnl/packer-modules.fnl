(module packer-modules
  {:require {packer plugins.packer}})

(def- spec
  [{:name "wbthomason/packer.nvim"
    :opt true}
   {:name "nvim-lua/plenary.nvim"}
   {:name "nvim-lua/telescope.nvim"
    :requires [["nvim-lua/popup.nvim"] ["nvim-lua/plenary.nvim"]]}
   {:name "tjdevries/colorbuddy.nvim"}])

(packer.config spec)
