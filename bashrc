#!/usr/bin/env bash

# ~/.bashrc
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

if [[ "${-}" != *i* ]]; then
    return
fi

shopt -s checkwinsize
shopt -s dotglob
shopt -s no_empty_cmd_completion

shopt -s -o pipefail

HISTCONTROL='ignoredups'
HISTTIMEFORMAT='[%F %a %T %z] '

PROMPT_DIRTRIM=3

unset -v PROMPT_COMMAND

_with_max_manwidth_80 () {
    if [[ ${#} -lt 1 ]]; then
        return 1
    fi
    local cmd="${1}"
    shift
    if [[ -n "${MANWIDTH}" || -z "${COLUMNS}" ]]; then
        command "${cmd}" "${@}"
    else
        if [[ ${COLUMNS} -ge 80 ]]; then
            MANWIDTH=80 command "${cmd}" "${@}"
        elif [[ ${COLUMNS} -ge 66 ]]; then
            # Some versions of 'man' use 80 columns if COLUMNS (or
            # MANWIDTH) is in the range 66-80, which makes the manual
            # page display look bad in windows with 66-79 columns.  In
            # these situations, tell 'man' that there are only 65
            # columns.
            MANWIDTH=65 command "${cmd}" "${@}"
        else
            command "${cmd}" "${@}"
        fi
    fi
}

alias cgrep='command grep --color=always'
alias git='_with_max_manwidth_80 git'
alias grep='grep --color=auto'
alias info='_with_max_manwidth_80 info'
alias ll='ls -l'
alias ls='ls -A -b'
alias man='_with_max_manwidth_80 man'
alias which-command='whence -a -v'

x="${HOME}/.bashrc.local"
if [[ -r "${x}" && -f "${x}" ]]; then
    source "${x}"
fi
unset -v x
