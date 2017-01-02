# .bashrc
# Raghu Rao
# Unlike .profile, this file is expected for use only with bash, so bashisms
# should be fine to use here.

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

if ! alias cgrep &>/dev/null; then alias cgrep='grep --color=always'; fi
if ! alias ls &>/dev/null; then alias ls='ls -F'; fi
if ! alias ll &>/dev/null; then alias ll='ls -l'; fi

# For reasons I do not yet understand, C-l does not clear the screen in tmux
# on Slackware.  So, bind C-l to tput clear, which works(?!).
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
