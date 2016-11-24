# .bashrc
# Raghu Rao
# Unlike .profile, this file is expected for use only with bash, so bashisms
# should be fine to use here.

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

if ! type -a cgrep >/dev/null 2>&1; then alias cgrep='grep --color=always'; fi
if ! alias ls >/dev/null 2>&1; then alias ls='ls -F'; fi
