# Prerequisites
You will need to install brew, hyper, neovim, and tmux

## macOS



```bash
brew install tmux
brew install python
brew install python3
brew install ruby
brew install reattach-to-user-namespace
brew install neovim/neovim/neovim
pip install neovim
pip3 install neovim
gem install neovim
```

Clone folder in home directory.

```bash
git clone https://github.com/berkeleytrue/dotfiles.git ~/.vim
```


While in your home directory enter the following,

```bash
curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
ln -s ~/.vim/dotfiles/vimrc ~/.vimrc
rm ~/.profile && ln -s ~/.vim/dotfiles/profile ~/.profile
ln -s ~/.vim/dotfiles/tmux.conf ~/.tmux.conf
ln -s ~/.vim/dotfiles/eslintrc ~/.eslintrc
ln -s ~/.vim/dotfiles/vintrc.yml ~/.vintrc.yml
ln -s ~/.vim/dotfiles/hyper.js ~/.hyper.js
ln -s ~/.vim/dotfiles/init.vim ~/.config/nvim/init.vim
ln -s ~/.vim/dotfiles/modules ~/.config/nvim/modules
ln -s ~/.vim/spell ~/.config/nvim/spell
source ~/.profile
```

This will create symbolic links in the home dir and the config
directory.

Now open up neovim and run the plugin installer, this should take a while.

```
nvim
```

And the run the following commands one after the other

```
:PlugInstall
:UpdateRemotePlugins
```

Now [install hyper](https://hyper.is/)
