# at some point, this should maybe be set by an m4 macro during install
export MY_ZSH_HOME="$HOME/.dotfiles/zsh"

# For debugging my shite scripts
_logz() {
  local logfile="$HOME/zsh-log.txt"
  printf "%s:\n" "$(date)" >> "$logfile"
  printf "$1\n" ${@[2,-1]} | sed 's/^/  /' >> "$HOME/zsh-log.txt"
}


[[ -f $MY_ZSH_HOME/secrets.zsh ]] && . $MY_ZSH_HOME/secrets.zsh
[[ -f "$HOME/.profile" ]] && . "$HOME/.profile"
