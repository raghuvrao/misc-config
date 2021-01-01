#!/usr/bin/env bash

# ~/.bashrc
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

if [[ "${-}" != *i* ]]; then return; fi

shopt -s checkwinsize dotglob no_empty_cmd_completion

shopt -u -o emacs

shopt -s -o pipefail vi

HISTCONTROL='ignoredups'
HISTTIMEFORMAT='[%F %a %T %z] '

# See the file SETTING-TITLES for information about setting terminal
# emulator window/tab titles, and icon names.
PS1=''
case "${TERM}" in
    (xterm*|rxvt*)
        PS1+='\[\e]0;\H:\w\a\]'
        ;;
    (tmux*|screen*)
        PS1+='\[\e]0;\H:\w\a\]'
        PS1+='\[\ek\H:\w\e\\\]'
        ;;
esac
PS1+='(\H) (\l) (${?}) (\w)\n\$ '

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
