#!/usr/bin/env bash

# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>
#
# bashisms are allowed in this file.

if [[ "${-}" != *i* ]]; then return; fi

shopt -s checkwinsize no_empty_cmd_completion

set -o pipefail

HISTCONTROL='ignoredups'

PS1='\H:\w\$ '

# See the file SETTING-TITLES for information about setting terminal emulator
# window/tab titles, and icon names.
case "${TERM}" in
    (xterm*|rxvt*)
        PS1='\[\033]0;\H:\w\007\]'"${PS1}"
        ;;
    (tmux*|screen*)
        PS1='\[\033]0;\H:\w\007\]\[\033k\H:\w\033\134\]'"${PS1}"
        ;;
esac

unset -v PROMPT_COMMAND

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
