clone folder in home directory.

```bash
git clone https://github.com/Berkeley110/dotfiles.git ./.vim
```
The file directory will be at:
```bash
~/.vim 
```
While in your home directory enter the following,
```bash
ln -s ~/dotfiles/vimrc ~/.vimrc 
ln -s ~/dotfiles/bash_aliases ./.bash_aliases
``` 
This will create symbolic links in the home dir for vim and bash.
Now we
 ```bash
cd ~/.vim
``` 
And perform the following,
```bash
git submodule init 
git submodule update 
```
This will initialize and download the submodules into ~/.vim/bundle/

To add your own vim plugins follow the following formula:
```bash
cd ~/.vim
git submodule add <insert your plugins git url here> bundle/<name of plugin>
```
This not only adds your plugin but adds its path to the .gitmodule file. Now when 
you want to check for updates you can just:
```bash
cd ~/.vim
git submodule sync
```
This will update all the plugins if needed.

A summery of what is in the bundle folder

Command-T: plug-in provides an extremely fast, intuitive mechanism for
opening files and buffers with a minimal number of keystrokes.
Delmitmate: Automatic closing of quotes, parenthesis, brackets, etc., besides some other related features
OmniCppComplete: cpp completion
Syntastic: A syntax checking plugin for Vim that runs files through external syntax checkers 
	and displays any resulting errors to the user. 
Rainbow_Parentheses: Adds colors to parentheses making them easier to read.
solarized: Provided color for vim editor.
supertab: Allows you to use <Tab> for all your insert completion needs (:help ins-completion).
vim-LaTeX: LaTeX bindings for document generation

HTML/CSS/JS plugins

Markdown: Syntax for Markdown (html prose)
HamleSass: Syntax for Haml Sass(html/css preprossecors)
vim-coffeescript: CoffeeScript support to vim. It covers syntax, indenting, compiling, and more.
Jade: Syntax highlighting for html prepross.
Javascript: javascript syntax
nodejs: Node snippets
backbone: backbone snippets
