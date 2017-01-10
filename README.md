# Prerequisites
You will need to install brew, hyper, neovim, and tmux

## macOS

[install hyper](https://hyper.is/)

```bash
brew install neovim/neovim/neovim
brew install tmux
```

Clone folder in home directory.

```bash
git clone https://github.com/berkeleytrue/dotfiles.git ~/.vim
```


While in your home directory enter the following,

```bash
ln -s ~/.vim/dotfiles/vimrc ~/.vimrc
ln -s ~/.vim/dotfiles/profile ~/.profile
ln -s ~/.vim/dotfiles/tmux.conf ~/.tmux.conf
ln -s ~/.vim/dotfiles/eslintrc ~/.eslintrc
ln -s ~/.vim/dotfiles/vintrc.yml ~/.vintrc.yml
ln -s ~/.vim/dotfiles/hyper.js ~/.hyper.js
ln -s ~/.vim/dotfiles/init.vim ~/.config/nvim/init.vim
ln -s ~/.vim/dotfiles/modules ~/.config/nvim/modules
ln -s ~/.vim/spell ~/.config/nvim/spell
```

This will create symbolic links in the home dir and the config
directory.
