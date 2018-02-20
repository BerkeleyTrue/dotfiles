# Bash helper alias
alias ..='cd ..'
alias ...='cd .. && cd ..'
alias ll='ls -aFlh'
alias v='nvim'
alias vi='nvim'
alias vim='nvim'
alias tmux="TERM=screen-256color-bce tmux -2"
## Will make parent directories if they don't exist
## Also verbose
alias mkdir="mkdir -pv"
alias whatismyip="curl http://ipecho.net/plain; echo"
## Make file executable
alias chmodx='chmod 755'
## Make file read/write-able
alias chmodrw='chmod 644'
## print bash function declaration in console
## Used to make sure profile changes are reflecting
alias echodec='declare -f'

#quick edits
alias vprofile='nvim ~/.profile'
alias sprofile='source ~/.profile'
alias vneo='nvim ~/.config/nvim/init.vim'
alias vlint='nvim ~/.eslintrc'

#node aliases
alias :mongo='mongod --dbpath ~/data/db'

alias npmig='sudo npm install -g'
alias npmug='sudo npm uninstall -g'

alias npmid='npm install --save-dev'
alias npmud='npm uninstall --save-dev'

alias npmis='npm install --save'
alias npmus='npm uninstall --save'

alias npms='npm run start'
alias npmt='npm run test'
alias npml='npm run lint'

#quickmoves
alias cdnode='cd ~/node'
alias cdf='cd ~/node/freecodecamp/actual'
alias cdrt='cd ~/node/react'
alias cdrdx='cd ~/node/redux'
alias cdrwr='cd ~/node/rwr'
alias cdvim='cd ~/.vim'
alias cdsnippets='cd ~/.config/nvim/plugged/berkeleys-snippet-emporium'
