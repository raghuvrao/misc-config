#!/bin/bash

# ~/.bashrc
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

p='/etc/bashrc'
if [[ -f "${p}" && -r "${p}" ]]; then
    source "${p}"
fi
unset -v p

umask 077

if [[ "${-}" != *i* ]]; then
    return
fi

shopt -s checkwinsize
shopt -s dotglob
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
