# .bashrc
# Raghu Rao
# Unlike .profile, this file is expected for use only with bash, so bashisms
# should be fine to use here.

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

if ! alias cgrep &>/dev/null; then alias cgrep='grep --color=always'; fi
if ! alias ls &>/dev/null; then alias ls='ls -F'; fi
if ! alias ll &>/dev/null; then alias ll='ls -l'; fi
