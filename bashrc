#!/usr/bin/env bash

# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>
#
# Unlike .profile, this file is for use only with bash, so bashisms (e.g.
# ${HOSTNAME} and [[ ... ]]) may be used in this file.

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

# Make bash update its idea of the window's size after each command.
shopt -s checkwinsize

# Make pipelines return the exit status of the most recent (rightmost) command
# in them that exited with a non-zero exit status, instead of the exit status
# of the final command.
shopt -o -s pipefail

# Do not perform completion when completion is attempted on an empty line.
shopt -s no_empty_cmd_completion

# Do not save lines matching previous history entry.
HISTCONTROL='ignoredups'
PROMPT_DIRTRIM=3

# Set prompt and terminal emulator window/tab titles and icon names.  See
# SETTING-TITLES for more information.
PS1='\h:\w\$ '
case "${TERM}" in
	xterm*|rxvt*)
		PS1='\[\033]0;\h:\w\007\]'"${PS1}"
		;;
	screen*)
		PS1='\[\033]0;\h:\w\007\]\[\033k\h:\w\033\134\]'"${PS1}"
		;;
esac

unset -v PROMPT_COMMAND

# In Slackware, when running bash, readline's clear-screen function (bound to
# C-l by default) does not seem to work as expected for certain types of
# terminals (e.g.  xterm-256color, screen*, rxvt*).  For these terminal types,
# the command `tput clear' works as expected.  So, work around the problem by
# binding C-l to `tput clear', until I find a better solution.
if [[ "${TERM}" =~ xterm-.*|screen.*|rxvt.* ]]; then
	f="/etc/slackware-version"
	if [[ -r "${f}" ]]; then
		read -r first_line <"${f}" &>/dev/null
		if [[ "${first_line}" =~ ^[Ss]lackware ]]; then
			builtin bind -r "\C-l"
			builtin bind -x '"\C-l": tput clear'
		fi
		unset -v first_line
	fi
	unset -v f
fi
