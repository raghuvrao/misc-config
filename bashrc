# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>
#
# Unlike .profile, this file is for use only with bash, so bashisms (e.g.
# ${HOSTNAME} and [[ ... ]]) may be used in this file.

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

# Source system-wide settings file.
if [[ -r /etc/bashrc ]]; then . /etc/bashrc; fi

# Set up a few aliases I like.
alias colorgrep='grep --color=always'
alias ll='ls -l'

# Make sure bash updates its idea of window size after each command.
shopt -s checkwinsize

# Set xterm title.  See accompanying document SETTING-TITLES for a brief
# discussion.
unset PROMPT_COMMAND
s1='"\e]0;${HOSTNAME%%.*}:${PWD/#${HOME}/\~}\a"'
s2='"\ek${HOSTNAME%%.*}:${PWD/#${HOME}/\~}\e\\"'
if [[ "${TERM}" == xterm* ]]; then
    PROMPT_COMMAND="printf ${s1}"
elif [[ "${TERM}" == screen* ]]; then
    PROMPT_COMMAND="printf ${s1}${s2}"
fi
unset s1 s2

# In Slackware, when running bash, readline's clear-screen function (bound to
# C-l by default) does not seem to work as expected for certain TERMs (e.g.
# xterm-256color, screen-256color and screen).  For these TERMs, the command
# 'tput clear' works as expected.  So, work around the problem by rebinding
# C-l to 'tput clear' until I find a proper solution.
if [[ "${TERM}" =~ xterm-.*|screen.* ]]; then
    f="/etc/slackware-version"
    if [[ -r "${f}" ]]; then
        read -r first_line <"${f}" &>/dev/null
        if [[ "${first_line}" =~ ^[Ss]lackware ]]; then
            builtin bind -r "\C-l"
            builtin bind -x '"\C-l": tput clear'
        fi
    fi
    unset f
fi
