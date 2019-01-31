

# NVM directory
if [ -d "$HOME/.nvm" ]; then
   export NVM_DIR="$HOME/.nvm"
   . "/usr/local/opt/nvm/nvm.sh"
fi

alias config='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# Alias
alias ..="cd .."
alias cd.="cd .."
alias cd..="cd .."
alias em="emacs -nw"
alias nano=em
alias grep="grep --color"
alias rm="rm -i"
alias gpg="gpg2"
alias org='git --git-dir=$HOME/Org/.git/'