#!/usr/bin/env bash

# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>
#
# bashisms are allowed in this file.

if [[ "${-}" != *i* ]]; then return; fi

shopt -s checkwinsize no_empty_cmd_completion

set -o pipefail

HISTCONTROL='ignoredups'
HISTTIMEFORMAT='[%F %a %T %z] '

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

# Slackware sets MANPATH to /usr/local/man:/usr/man:${JAVA_HOME}/man in
# /etc/profile* (even though /etc/man_db.conf is configured satisfactorily),
# and it was causing more problems than solving anything for me.  E.g. I use
# a version of bash that I compiled myself and installed in /usr/local.  So,
# when I do `man bash', I should see the bash manual page located in
# /usr/local/share; instead, man pulls up the system-bash manual page from
# /usr/share, which does not match the version of bash I am using.  Unsetting
# MANPATH solves this problem, and makes man work as expected.
unset -v MANPATH

alias cgrep='grep --color=always'
alias grep='grep --color=auto'
alias ls='ls --almost-all --tabsize=0 --escape'
