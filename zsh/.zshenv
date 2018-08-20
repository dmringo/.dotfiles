# at some point, this should maybe be set by an m4 macro during install export
MY_ZSH_HOME="$HOME/.dotfiles/zsh"

[[ -e $MY_ZSH_HOME/secrets.zsh ]] && . $MY_ZSH_HOME/secrets.zsh
[[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'

alias l='ls -lh --color="auto" --group-directories-first'
alias la='ls -lhar --color="auto" --group-directories-first'

case $(uname -s) in
    Linux*)
        source $MY_ZSH_HOME/linuxenv.zsh
        ;;
    *)
        ;;
esac
