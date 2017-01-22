# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>

umask 077

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

p='/etc/bashrc'
if [[ -f "${p}" && -r "${p}" ]]; then
    source "${p}"
fi

HISTCONTROL='ignoreboth'
HISTFILESIZE=20000
HISTSIZE="${HISTFILESIZE}"
HISTTIMEFORMAT='%F %a %T %Z(UTC%z) '

# Set up a handy alias to force grep to color my output (useful when piping
# grep output through less -R).
if ! type -a cgrep &>/dev/null; then alias cgrep='grep --color=always'; fi

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

unset -v p
