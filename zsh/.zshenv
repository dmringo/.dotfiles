# at some point, this should maybe be set by an m4 macro during install
export MY_ZSH_HOME="$HOME/.dotfiles/zsh"

cmd_exists() {
  command -v "$1" 2>&1 > /dev/null
}

# For debugging my shite scripts
_logz() {
  local logfile="$HOME/zsh-log.txt"
  printf "%s:\n" "$(date)" >> "$logfile"
  printf "$1\n" ${@[2,-1]} | sed 's/^/  /' >> "$HOME/zsh-log.txt"
}


[[ -f $MY_ZSH_HOME/secrets.zsh ]] && . $MY_ZSH_HOME/secrets.zsh
[[ -f ~/.profile ]] && emulate sh -c 'source ~/.profile'


alias l='ls -lh --color="auto" --group-directories-first'
alias la='ls -lhar --color="auto" --group-directories-first'
