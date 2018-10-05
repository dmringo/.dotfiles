# linux-sepecific functions and aliases
# intended to be sourced from .zshenv

alias o='xdg-open'

clip() {
  case "$1" in
    f|filter|i|in|o|out )
      mode="$1"
      ;;
    "" )
      mode=i
      ;;
    * )
      printf "Unrecognized option: %s\n" "$1"
      return 1
      ;;
  esac
  xclip -"$mode" -selection clipboard "$@"
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
