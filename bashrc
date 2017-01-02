# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>

umask 077

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

HISTCONTROL='ignoreboth'
HISTFILESIZE=20000
HISTSIZE="${HISTFILESIZE}"
HISTTIMEFORMAT='%F %a %T %Z(UTC%z) '

if ! alias cgrep &>/dev/null; then alias cgrep='grep --color=always'; fi
if ! alias ls &>/dev/null; then alias ls='ls -F'; fi
if ! alias ll &>/dev/null; then alias ll='ls -l'; fi

# For reasons I do not yet understand, C-l does not clear the screen in
# bash+tmux on Slackware (i.e. the readline function clear-screen (which is
# bound to C-l by default) does not seem to work as expected when in tmux on
# Slackware).  However, the command 'tput clear' magically works.  So, work
# around the problem for now by binding C-l to 'tput clear'.
f="/etc/slackware-version"
if [[ -n "${TMUX}" && -r "${f}" ]]; then
    while read line; do
        if [[ "${line}" =~ ^[Ss]lackware ]]; then
            builtin bind -r "\C-l"
            builtin bind -x '"\C-l": tput clear'
            break
        fi
    done < "${f}"
fi
unset f
