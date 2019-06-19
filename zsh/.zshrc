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

zstyle :compinstall filename '$ZDOTDIR/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}"/zsh/history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd extendedglob nomatch notify incappendhistory
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install


# make the directory for the history file if it doesn't exist
histdir="$(dirname $HISTFILE)"
[[ -d $histdir ]] || mkdir -p $histdir


# let zsh use completions defined for bash
autoload -U +X bashcompinit && bashcompinit

# Use bash-style word boundaries (i.e. anything not alphanumeric)
autoload -U select-word-style && select-word-style bash

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

#--------- prompt setup

autoload -U promptinit && promptinit
# spaceship is 3rd party, all others are bundled with Zsh
my_prompts=(spaceship clint elite adam bart)
sys_prompts=($(prompt -l | sed -n 2p))

for p in $my_prompts
do
  # (r)$p in the subscript says "give me the value of the element that matches
  # the pattern $p"
  found_prompt=${sys_prompts[(r)$p]}
  if [[ -n $found_prompt ]]
  then
    # TODO: There's a better way to do this for sure
    case $found_prompt in
      spaceship )
        SPACESHIP_DIR_TRUNC_PREFIX="…/"
        SPACESHIP_DIR_TRUNC_REPO="false"
        SPACESHIP_CONDA_PREFIX="["
        SPACESHIP_CONDA_SUFFIX="]"
        SPACESHIP_CONDA_SYMBOL=""
        SPACESHIP_PROMPT_ORDER=(dir user host git conda exec_time
                                line_sep exit_code char)
        ;;
    esac

    prompt $p
    break
  fi
done





# Haskell stack
if cmd_exists stack
then
  # note that this requires the bashcompinit module loaded to work
  eval "$(stack --bash-completion-script stack)"
fi

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

case "$(ls --version 2>/dev/null | head -n 1)" in
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

# Source a local rc file (for interactive use)
[[ -f $ZDOTDIR/localrc.zsh ]] && . $ZDOTDIR/localrc.zsh
