# Checking for dumb terminals - makes Tramp work when editing remote files
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return;

# pretty sure fpath has to be modified before compinit is called
if have_brew
then
  fpath=("$(brew --prefix)/share/zsh/site-functions" $fpath)
fi

# cd to recent dirs
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

# The following lines were added by compinstall
zstyle ':completion:*' completer \
       _expand _complete _ignored _match _approximate _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' format 'Completing %d ...'
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list \
       'm:{[:lower:]}={[:upper:]}' \
       'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' \
       'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' menu select=0
zstyle ':completion:*' prompt '%e errors corrected for completion'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' substitute 1
zstyle ':completion:*' word true

# This is what lets you navigate with e.g. up-line-or-history in completion
# menus.  Usually has to be triggered by entering a menu for ambiguous
# completion with an extra <tab>.
zstyle ':completion:*' menu selection

zstyle :compinstall filename '/$HOME/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}"/zsh/history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd extendedglob nomatch notify
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install


# let zsh use completions defined for bash
autoload -U +X bashcompinit && bashcompinit


# smarter functions for <M-n> and <M-p>
# These match all text entered in command line while searching
autoload -U up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

keybinds=(
  # <M-p>
  "^[p"   up-line-or-beginning-search
  # <M-n>
  "^[n"   down-line-or-beginning-search
  # Home key
  "^[[H"  beginning-of-line
  # End key
  "^[[F"  end-of-line
  # Delete key
  "^[[3~" delete-char-or-list
)

bindkey $keybinds

# Tell gpg about the TTY.  I think this only makes sense in an interactive
# session.  I also think this is necessary when GPG assumes no TTY, as is the
# case when $HOME/.gnupg/gpg.conf has 'no-tty' specified.
export GPG_TTY="$(tty)"

# Check if zplug is installed
export ZPLUG_ROOT="${XDG_DATA_HOME:-$HOME/.local/share}/zplug"

if [[ ! -d $ZPLUG_ROOT ]]
then
  git clone https://github.com/zplug/zplug $ZPLUG_ROOT
  source $ZPLUG_ROOT/init.zsh && zplug --self-manage
fi

# Essential
source $ZPLUG_ROOT/init.zsh

# Make sure to use double quotes to prevent shell expansion
zplug "zsh-users/zsh-syntax-highlighting", defer:2

zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, from:github, as:theme

# Using direnv to conditionally modify the spaceship sections would be nice
# (rather than enabling all the extras, only load them when I know I'll need
# them).  This would have a noticeable performance improvement -- the set I use
# seems to be much faster than the default.  This isn't currently possible with
# direnv though, so it may be worth looking into a custom CD hook.  For now, the
# basic set of components is just fine
SPACESHIP_DIR_TRUNC_PREFIX="…/"
SPACESHIP_DIR_TRUNC_REPO="false"
SPACESHIP_CONDA_PREFIX="["
SPACESHIP_CONDA_SUFFIX="]"
SPACESHIP_CONDA_SYMBOL=""
SPACESHIP_PROMPT_ORDER=(dir user host git conda exec_time line_sep exit_code char)

# oh-my-zsh git plugin is nice
zplug "plugins/git", from:oh-my-zsh, if:'cmd_exists git'

if cmd_exists git
then
  # in the style of the omz git aliases
  alias gsur='git submodule update --recursive'
  alias gcfo='git config --list --show-origin'
fi


# Haskell stack
if cmd_exists stack
then
  # note that this requires the bashcompinit module loaded to work
  eval "$(stack --bash-completion-script stack)"
fi

# pandoc completion
zplug "srijanshetty/zsh-pandoc-completion", if:'cmd_exists pandoc'

# GTK settings manager completion
zplug "jmatsuzawa/zsh-comp-gsettings", if:'cmd_exists gsettings'

# Keybase.io
zplug "rbirnie/oh-my-zsh-keybase", if:'cmd_exists keybase'

# Homebrew
zplug "vasyharan/zsh-brew-services", if:'cmd_exists brew'

# Alias ripgrep to use a config file
if cmd_exists rg
then
  alias rgi="rg -i"
  rg_conf="${XDG_CONFIG_HOME:-$HOME/.config}/ripgrep/config"
  if [[ -f "$rg_conf" ]]
  then
    # TODO: Don't do this. Just export the var and deal with it.
    # also move this. It's useful in all shells
    alias rg="RIPGREP_CONFIG_PATH=$rg_conf rg"
  fi
fi

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

# if we have direnv, get its hook setup
cmd_exists direnv && eval "$(direnv hook zsh)"

if cmd_exists docker
then
  alias dkr='docker'
  alias dkx='docker exec -i -t'
  alias dke='docker exec'
  alias dkps='docker ps'

  if cmd_exists docker-compose
  then
    alias dkc='docker-compose'
    alias dkcx='docker-compose exec'
    alias dkcu='docker-compose up -d'
    alias dkcU='docker-compose up'
    alias dkcd='docker-compose down'
  fi
fi



case "$(ls --version | head -n 1)" in
  *GNU*)
    extopts=" --color --group-directories-first"
    ;;
  *)
    extopts=""
    ;;
esac

alias l="ls -lh $extopts"
alias la="ls -lha $extopts"
alias ll="ls -lhA $extopts"
