# Checking for dumb terminals - makes Tramp work when editing remote files
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return;

# pretty sure fpath has to be modified before compinit is called
if have_brew
then
  fpath=("$(brew --prefix)/share/zsh/site-functions" $fpath)
fi

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

# let zsh use completions defined for bash
autoload -U +X bashcompinit && bashcompinit

# Tell gpg about the TTY.  I think this only makes sense in an interactive
# session.  I also think this is necessary when GPG assumes no TTY, as is the
# case when $HOME/.gnupg/gpg.conf has 'no-tty' specified.
export GPG_TTY="$(tty)"

# Check if zplug is installed
export ZPLUG_HOME="$MY_ZSH_HOME/.zplug"

if [[ ! -d $ZPLUG_HOME ]]
then
  git clone https://github.com/zplug/zplug $ZPLUG_HOME
  source $ZPLUG_HOME/init.zsh && zplug --self-manage
fi

# Essential
source $ZPLUG_HOME/init.zsh

# Make sure to use double quotes to prevent shell expansion
zplug "zsh-users/zsh-syntax-highlighting", defer:2

# required for `pure` prompt
zplug "mafredri/zsh-async", from:github
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme

# l with fancy colors and git info
zplug "supercrabtree/k", hook-load:"alias k='k -h'"

# tell me if there's a faster way to run some command
zplug "djui/alias-tips"

# oh-my-zsh git plugin is nice
zplug "plugins/git", from:oh-my-zsh, if:'cmd_exists git'

# alias in the style of the omz git aliases
alias gsur='git submodule update --recursive'

# Haskell stack
if cmd_exists stack
then
  eval "$(stack --bash-completion-script stack)"
fi

# pandoc completion
zplug "srijanshetty/zsh-pandoc-completion", if:'cmd_exists pandoc'

# GTK settings manager
zplug "jmatsuzawa/zsh-comp-gsettings", if:'cmd_exists gsettings'

# Keybase.io
zplug "rbirnie/oh-my-zsh-keybase", if:'cmd_exists keybase'

# Homebrew
zplug "vasyharan/zsh-brew-services", if:'cmd_exists brew'

# ZSH scripting hints
zplug "joepvd/zsh-hints"

# Grep, but faster and .*ignore-aware
zplug "BurntSushi/ripgrep", as:command, use:'complete/_rg', if:'cmd_exists rg'

# ninja build system
zplug "ninja-build/ninja", as:command, use:"misc/zsh-completion", if:'cmd_exists ninja'

# Install packages that have not been installed yet
if ! zplug check --verbose
then
  printf "Install? [y/N]: "
  if read -q
  then
    echo; zplug install
  else
    echo
  fi
fi

zplug load

setopt extendedglob

# if we have direnv, get its hook setup
cmd_exists direnv && eval "$(direnv hook zsh)"

