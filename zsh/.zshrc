# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _match _approximate _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' format 'Completing %d ...'
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' menu select=0
zstyle ':completion:*' prompt '%e errors corrected for completion'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' substitute 1
zstyle ':completion:*' word true
zstyle :compinstall filename '/$HOME/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd extendedglob nomatch notify
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install

# Check if zplug is installed
export ZPLUG_HOME="$MY_ZSH_HOME/.zplug"

if [[ ! -d $ZPLUG_HOME ]]; then
  git clone https://github.com/zplug/zplug $ZPLUG_HOME
  source $ZPLUG_HOME/init.zsh && zplug update --self
fi

# Essential
source $ZPLUG_HOME/init.zsh

# Make sure to use double quotes to prevent shell expansion
zplug "zsh-users/zsh-syntax-highlighting", defer:2

# Jump around by Frecency. Fork of rupa/z with better completion
export _Z_CMD=j
zplug "knu/z", use:z.sh, defer:2

# my own fancy prompt and theme
zplug "$MY_ZSH_HOME", from:local, as:theme, use:"dmr.zsh-theme"


# l with fancy colors and git info
zplug "supercrabtree/k"

# tell me if there's a faster way to run some command
zplug "djui/alias-tips"

# oh-my-zsh git plugin is nice
zplug "plugins/git", from:oh-my-zsh

# Haskell stack
zplug "plugins/stack", from:oh-my-zsh

# pandoc completion
zplug "srijanshetty/zsh-pandoc-completion"

# Keybase.io
type keybase 2>&1 > /dev/null && zplug "rbirnie/oh-my-zsh-keybase"

# Homebrew
type brew 2>&1 > /dev/null && zplug "vasyharan/zsh-brew-services"

# ZSH scripting hints
zplug "joepvd/zsh-hints"

zplug "BurntSushi/ripgrep", as:command, use:'complete/_rg'

# Install packages that have not been installed yet
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    else
        echo
    fi
fi

zplug load

setopt extendedglob
