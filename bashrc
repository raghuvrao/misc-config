#!/bin/bash

# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>

umask 077

if [[ "${-}" != *i* ]]; then return; fi

p='/etc/bashrc'
if [[ -f "${p}" && -r "${p}" ]]; then
    source "${p}"
fi
unset -v p

shopt -s checkwinsize no_empty_cmd_completion

set -o pipefail

HISTCONTROL='ignoredups'
HISTFILESIZE=20000
HISTSIZE="${HISTFILESIZE}"
HISTTIMEFORMAT='%F %a %T %Z(UTC%z) '

# In Slackware, when running bash, readline's clear-screen function (typically
# bound to C-l) does not seem to work as expected for certain types of
# terminals.  It is /probably/ because Slackware's bash depends on libtermcap
# rather than ncurses/terminfo.  For these terminal types, the command `tput
# clear' works as expected.  So, work around the problem by binding C-l to
# `tput clear'.  The better (proper?) solution is probably to recompile bash
# with ncurses rather than termcap, but I am lazy, so I will just use the
# workaround I have for now.
if [[ "${TERM}" =~ xterm-.*|rxvt.*|screen.*|tmux.* ]]; then
    if shopt -q -o emacs; then
        bind -m emacs -r "\C-l"
        bind -m emacs -x '"\C-l": tput clear'
    elif shopt -q -o vi; then
        bind -m vi-command -r "\C-l"
        bind -m vi-command -x '"\C-l": tput clear'
        bind -m vi-insert -r "\C-l"
        bind -m vi-insert -x '"\C-l": tput clear'
    fi
fi

cgrep() { command grep --color=always "${@}"; }
grep() { command grep --color=auto "${@}"; }
