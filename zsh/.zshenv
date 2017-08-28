export GOPATH="$HOME/go"
export MY_ZSH_HOME="$HOME/.dot-files/zsh"

# github API rate limit?
export HOMEBREW_GITHUB_API_TOKEN=a986d55cf8916c44b2d9221b31e3349e3d3301f6

export PATH="\
$HOME/.local/bin:\
$GOPATH/bin:\
$HOME/.cabal/bin:\
$HOME/.rvm/bin:\
/usr/local/bin:\
/usr/bin:\
/bin:\
/usr/sbin:\
/sbin:\
/opt/X11/bin:\
/Library/TeX/texbin"

export EDITOR="emacs"

alias l='ls -lh --color="auto" --group-directories-first'
alias la='ls -lhar --color="auto" --group-directories-first'

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"