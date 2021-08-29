#!/bin/bash

# ~/.bashrc
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

_s ()
{
    if [[ -f "${1}" && -r "${1}" ]]; then
        source "${1}"
    fi
}

_s '/etc/bashrc'

umask 077

if [[ "${-}" != *i* ]]; then
    unset -f _s
    return
fi

shopt -s checkwinsize
shopt -s dotglob
shopt -s failglob
shopt -s no_empty_cmd_completion

shopt -s -o pipefail

HISTCONTROL='ignoredups'
HISTFILESIZE=20000
HISTSIZE="${HISTFILESIZE}"
HISTTIMEFORMAT='%F %a %T %Z(UTC%z) '

alias cgrep='command grep --color=always'
alias grep='grep --color=auto'
alias ll='ls -l'
alias ls='ls -A'

_s "${HOME}/.bashrc_local.bash"

unset -f _s
