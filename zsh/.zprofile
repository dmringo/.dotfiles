# anything in specified in ~/.profile (for [ba]sh, e.g.) should be replicated in
# a zsh login shell (which only reads ~/.zprofile)

_logz "in zprofile"
_logz "path is %s" $path

[[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'

_logz "just sourced .profile"
_logz "path is %s" $path

/usr/local/opt/coreutils/libexec/gnubin
/Users/dringo/.cabal/bin
/Users/dringo/.local/go/bin
/Users/dringo/.local/bin
/Users/dringo/.dotfiles/zsh/.zplug/bin
/Users/dringo/.cabal/bin
/Users/dringo/.local/go/bin
/Users/dringo/.local/bin
/usr/local/bin
/usr/bin
/bin
/usr/sbin
/sbin
/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_10
/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_10
