# .bashrc
# Raghu Rao

umask 077

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

HISTCONTROL='ignoreboth'
HISTFILESIZE=20000
HISTSIZE="${HISTFILESIZE}"
HISTTIMEFORMAT='%F %a %T %Z(UTC%z) '

if ! type -a cgrep >/dev/null 2>&1; then alias cgrep='grep --color=always'; fi
if ! alias ls >/dev/null 2>&1; then alias ls='ls -F'; fi
