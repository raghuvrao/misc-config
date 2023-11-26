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
shopt -s histappend
shopt -s no_empty_cmd_completion

shopt -s -o pipefail

HISTCONTROL='ignoredups'
HISTFILESIZE=20000
HISTSIZE="${HISTFILESIZE}"
HISTTIMEFORMAT='%F %a %T %Z(UTC%z) '

q ()
{
    unset HISTFILE
    if [[ -n "${HISTFILE+y}" ]]; then
        printf 'HISTFILE is still set: <%s>\n' "${HISTFILE}" >&2
        return 1
    fi
    exit
}

alias cgrep='command grep --color=always'
alias grep='grep --color=auto'
alias ll='ls -l'
alias ls='ls -A -p'
alias pr='pr -t'

_s "${HOME}/.bashrc_local.bash"

unset -f _s
