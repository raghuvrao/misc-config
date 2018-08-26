#!/usr/bin/env bash

# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>
#
# bashisms are allowed in this file.

if [[ "${-}" != *i* ]]; then return; fi

shopt -s checkwinsize no_empty_cmd_completion

set -o pipefail

HISTCONTROL='ignoredups'

PS1='\h:\w\$ '

# See the file SETTING-TITLES for information about setting terminal emulator
# window/tab titles, and icon names.
case "${TERM}" in
    xterm*|rxvt*)
        PS1='\[\033]0;\h:\w\007\]'"${PS1}"
        ;;
    screen*)
        PS1='\[\033]0;\h:\w\007\]\[\033k\h:\w\033\134\]'"${PS1}"
        ;;
esac

PROMPT_DIRTRIM=3

unset -v PROMPT_COMMAND

# In Slackware, when running bash, readline's clear-screen function (typically
# bound to C-l) does not seem to work as expected for certain types of
# terminals.  For these terminal types, the command `tput clear' works as
# expected.  So, work around the problem by binding C-l to `tput clear', until
# I find a better solution.
if [[ "${TERM}" =~ xterm-.*|screen.*|rxvt.* ]]; then
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
