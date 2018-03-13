# Prerequisites
You will need to install hyper, karabiner-elements, and tmux

## macOS


```bash
# installs homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install git
brew install tig
brew install tmux
brew install python
brew install python3
brew install ruby
brew install reattach-to-user-namespace
brew install neovim/neovim/neovim
brew install node
brew install fzf
brew install lpass
brew install universal-ctags
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
/usr/local/opt/fzf/install
mkdir -p ~/.config/karabiner
mkdir -p ~/.config/nvim
mkdir -p ~/.tmux/tmux-plugins
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim;
rm ~/.profile
# ln -svhf creates a symbolic link verbosely.
# Will unlink files if they already exist
ln -svhf ~/.vim/dotfiles/czrc.json      ~/.czrc
ln -svhf ~/.vim/dotfiles/profile        ~/.profile
ln -svhf ~/.vim/dotfiles/tmux.conf      ~/.tmux.conf
ln -svhf ~/.vim/dotfiles/eslintrc       ~/.eslintrc
ln -svhf ~/.vim/dotfiles/zsh.zshrc      ~/.zshrc
ln -svhf ~/.vim/dotfiles/vintrc.yml     ~/.vintrc.yml
ln -svhf ~/.vim/dotfiles/hyper.js       ~/.hyper.js
ln -svhf ~/.vim/dotfiles/init.vim       ~/.config/nvim/init.vim
ln -svhf ~/.vim/spell                   ~/.config/nvim/spell
ln -svhf ~/.vim/dotfiles/modules        ~/.config/nvim/modules
ln -svhf ~/.vim/dotfiles/karabiner.json ~/.config/karabiner/karabiner.json
source ~/.profile
```

Now install global npm packages required for git workflow and npm publishing
```bash
npminstallpre
```

This will create symbolic links in the home dir and the config
directory.

Now open up neovim and run the plugin installer, this should take a while.

```
nvim -c "PlugInstall | UpdateRemotePlugins"
```


Install [patched nerd fonts](https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DroidSansMono/complete/Droid%20Sans%20Mono%20for%20Powerline%20Nerd%20Font%20Complete%20Mono.otf)
