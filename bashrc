# .bashrc
# Raghu Rao

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
