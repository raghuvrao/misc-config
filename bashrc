# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>

umask 077

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

HISTCONTROL='ignoreboth'
HISTFILESIZE=20000
HISTSIZE="${HISTFILESIZE}"
HISTTIMEFORMAT='%F %a %T %Z(UTC%z) '

if ! type -a cgrep &>/dev/null; then alias cgrep='grep --color=always'; fi

shopt -s checkwinsize

# For reasons I do not yet understand, C-l (by default, bound to readline's
# clear-screen function) does not clear the screen in bash on Slackware for
# some terminal types (e.g. screen, screen-256color and xterm-256color).
# However, on these terminal types, the command 'tput clear' works.  So, work
# around the problem for now by binding C-l to 'tput clear'.
f="/etc/slackware-version"
if [[ -r "${f}" && "${TERM}" =~ xterm\-.*|screen.* ]]; then
    read -r first_line <"${f}" &>/dev/null
    if [[ "${first_line}" =~ ^[Ss]lackware ]]; then
        builtin bind -r "\C-l"
        builtin bind -x '"\C-l": tput clear'
    fi
fi
unset f

unset PROMPT_COMMAND
