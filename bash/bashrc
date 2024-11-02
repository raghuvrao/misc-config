#!/bin/bash

# ~/.bashrc
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

# NOTE: If sourcing /etc/bashrc or similar, do it before setting the umask
# because /etc/bashrc or a script it sources could set a different umask.
umask 077

[[ "${-}" != *i* ]] && return

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

source "${HOME}/.bashrc_local.bash"
