#!/usr/bin/env bash

# ~/.bashrc
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>
#
# bashisms are allowed in this file.

if [[ "${-}" != *i* ]]; then return; fi

shopt -s checkwinsize dotglob no_empty_cmd_completion

set -o pipefail

HISTCONTROL='ignoredups'
HISTTIMEFORMAT='[%F %a %T %z] '

PS1='[\H ${?} \W] \$ '

# See the file SETTING-TITLES for information about setting terminal
# emulator window/tab titles, and icon names.
title_content='\H:\w'
osc_title='\e]0;'"${title_content}"'\a'
kseq='\ek'"${title_content}"'\e\\'
case "${TERM}" in
    (xterm*|rxvt*)
        PS1="\[${osc_title}\]${PS1}"
        ;;
    (tmux*|screen*)
        PS1="\[${kseq}\]\[${osc_title}\]${PS1}"
        ;;
esac
unset -v title_content osc_title kseq

unset -v PROMPT_COMMAND

alias cgrep='grep --color=always'
alias grep='grep --color=auto'
alias ls='ls -A -b'
alias ll='ls -l'
