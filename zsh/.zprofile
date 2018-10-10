# anything in specified in ~/.profile (for [ba]sh, e.g.) should be replicated in
# a zsh login shell (which only reads ~/.zprofile)

[[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'
