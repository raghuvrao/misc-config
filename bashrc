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

# Slackware sets MANPATH to /usr/local/man:/usr/man:${JAVA_HOME}/man in
# /etc/profile* (even though /etc/man_db.conf is configured satisfactorily),
# and it was causing more problems than solving anything for me.  E.g. I use
# a version of bash that I compiled myself and installed in /usr/local.  So,
# when I do `man bash', I should see the bash manual page located in
# /usr/local/share; instead, man pulls up the system-bash manual page from
# /usr/share, which does not match the version of bash I am using.  Unsetting
# MANPATH solves this problem, and makes man work as expected.
unset -v MANPATH

alias cgrep='command grep --color=always'
alias grep='grep --color=auto'
alias ls='ls -A'
