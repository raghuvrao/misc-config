# .bashrc
# Raghu Rao

umask 077

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

unset LS_COLORS PROMPT_COMMAND
unalias -a

# Some general guidelines to tell if an environment variable belongs in this
# file:
#   - it is used only by the shell or shell builtins (e.g. PS1, HISTSIZE,
#     GLOBIGNORE etc.)
#   - it could change between shell sessions (e.g. different values for PS1
#     based on the value of TERM)
# Exporting such environment variables is typically unnecessary.
GLOBIGNORE='.:..'
HISTCONTROL='ignoreboth'
HISTSIZE=10000
HISTFILESIZE=20000
HISTTIMEFORMAT='%F %a %T %Z '
PROMPT_DIRTRIM=3
PS1='(\H)\w\$ '

shopt -u -o posix vi
shopt -s -o emacs pipefail
shopt -u dotglob
shopt -s checkhash checkwinsize cmdhist extglob histappend histreedit \
         histverify no_empty_cmd_completion sourcepath

ls() { command ls -abF "${@}"; }

grep() { command grep --color=auto "${@}"; }

cgrep() { command grep --color=always "${@}"; }

t()
{
    local py=
    local tf='%s %F %a %T %Z(%z)'
    if [[ -z "${1}" ]]; then
        py='from datetime import datetime as dt; import pytz; print dt.now(pytz.timezone("US/Pacific")).strftime("'"${tf}"'");'
    elif [[ "${1}" =~ ^(\+|\-)?[0-9]+(\.[0-9]+)?$ ]]; then
        py='from datetime import datetime as dt; import pytz; print dt.utcfromtimestamp('"${1}"').replace(tzinfo=pytz.utc).astimezone(pytz.timezone("US/Pacific")).strftime("'"${tf}"'");'
    fi
    if [[ -n "${py}" ]]; then
        python -c "${py}"
    else
        return 1
    fi
}
