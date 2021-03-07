#!/bin/zsh

# ~/.zprofile
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

unset -v LESS
unset -v LESSOPEN
unset -v MANPATH

export VISUAL='vim'

# Force LibreOffice to use the generic Visual Components Library plugin.
export SAL_USE_VCLPLUGIN=gen

export mcfg="${HOME}/src/git/misc-config"
export mscr="${HOME}/src/git/misc-scripts"

typeset -U PATH path FPATH path

if (( ${#path} == 0 )); then
    path=(/usr/local/bin /usr/bin /bin)
fi

path+=(/usr/local/sbin /usr/sbin /sbin)

path=(${HOME}/.local/bin /opt/local/bin ${path})
