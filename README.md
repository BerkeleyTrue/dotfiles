# Berks Dotfiles Emporium

## Prerequisites

You will need to install hyper, karabiner-elements, and tmux

### macOS
```bash
# installs homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install
\ yadm
\ git
\ tig
\ tmux
\ python
\ python3
\ node
\ leiningen
\ rust
\ go
\ ruby
\ neovim/neovim/neovim
\ fzf
\ lpass
\ ack
\ antigen
pip install neovim
pip3 install neovim
gem install neovim
```

install these files using YADM

```bash
cd ~
yadm init
yadm remote add origin https://github.com/berkeleytrue/dotfiles.git
```

While in your home directory enter the following,

Now install global npm packages required for git workflow and npm publishing
```bash
npminstallpre
```
Now open up neovim and run the plugin installer, this should take a while.

```
nvim -c "PlugInstall | UpdateRemotePlugins"
```

Install [patched nerd fonts](https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DroidSansMono/complete/Droid%20Sans%20Mono%20for%20Powerline%20Nerd%20Font%20Complete%20Mono.otf)

### TODO

Create install script for osx/arch
