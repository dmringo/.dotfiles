# linux-sepecific functions and aliases
# intended to be sourced from .zshenv

alias o='xdg-open'

clip(){
    emulate -L zsh -o glob
    case ${1} in
        o|out ) ;&
        i|in )  ;&
        f|filter )
            mode=${1}
            ;;
        *    )
            printf "Unrecognized option: %s\n" $1
            return 1
            ;;
    esac
    xclip -${mode} -selection clipboard ${@:2}
}


if command -v brew 2>&1 > /dev/null
then
  # from linuxbrew profile examples, usually found at
  # /usr/share/doc/linuxbrew-wrapper/examples/profile
  ## for elf executables
  export PATH="/home/linuxbrew/.linuxbrew/bin:${PATH}"
  #
  ## for manpages
  export MANPATH="/home/linuxbrew/.linuxbrew/share/man:${MANPATH}"
  #
  ## for info pages
  export INFOPATH="/home/linuxbrew/.linuxbrew/share/info:${INFOPATH}"
fi
