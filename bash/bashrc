#!/bin/bash

# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>

umask 077

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

p='/etc/bashrc'
if [[ -f "${p}" && -r "${p}" ]]; then
	source "${p}"
fi
unset -v p

HISTCONTROL='ignoredups'
HISTFILESIZE=20000
HISTSIZE="${HISTFILESIZE}"
HISTTIMEFORMAT='%F %a %T %Z(UTC%z) '

# Make bash update its idea of the window's size after each command.
shopt -s checkwinsize

# Make pipelines return the exit status of the most recent (rightmost) command
# in them that exited with a non-zero exit status, instead of the exit status
# of the final command.
shopt -o -s pipefail

# Do not perform completion when completion is attempted on an empty line.
shopt -s no_empty_cmd_completion

# In Slackware, when running bash, readline's clear-screen function (bound to
# C-l by default) does not seem to work as expected for certain types of
# terminals (e.g.  xterm-256color, screen*, rxvt*).  For these terminal types,
# the command `tput clear' works as expected.  So, work around the problem by
# binding C-l to `tput clear', until I find a better solution.
if builtin shopt -q -o emacs || builtin shopt -q -o vi; then
	if [[ "${TERM}" =~ xterm-.*|screen.*|rxvt.* ]]; then
		builtin bind -m emacs -r "\C-l"
		builtin bind -m emacs -x '"\C-l": tput clear'
		builtin bind -m vi-command -r "\C-l"
		builtin bind -m vi-command -x '"\C-l": tput clear'
		builtin bind -m vi-move -r "\C-l"
		builtin bind -m vi-move -x '"\C-l": tput clear'
	fi
fi
