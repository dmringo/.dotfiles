# -*- mode: sh -*-
# stolen from/inspired by other themes: lukerandall, apple

autoload -Uz vcs_info
zstyle ':vcs_info:*' check-for-changes true

# display this when there are unstaged changes
zstyle ':vcs_info:*' unstagedstr '%F{magenta}*%f'
# display this when there are staged changes
zstyle ':vcs_info:*' stagedstr '%F{192}+%f'  
zstyle ':vcs_info:*' actionformats \
    '%F{white}[%F{036}%b%F{3}|%F{1}%a%c%u%F{white}]%f'
zstyle ':vcs_info:*' formats       \
    '%F{white}[%F{036}%b%c%u%F{white}]%f'
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'
zstyle ':vcs_info:*' enable git cvs svn
zstyle ':vcs_info:*' default ''

theme_precmd () { vcs_info }

local the_path='%F{032}%(5~|%-2~/%F{211}...%F{032}/%2~|%4~)%f '
local the_login='%F{111}%n@%m%f '
local the_vcs='${vcs_info_msg_0_}%f '
local the_tail='%(?.%F{036}.%F{160}[%?] )$%f '

if [[ $TERM = eterm-color ]]
then PROMPT="%1~ $ "
else PROMPT="$the_login$the_path$the_vcs$the_tail"
fi

autoload -U add-zsh-hook
add-zsh-hook precmd theme_precmd
