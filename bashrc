# .bashrc
# Raghu Rao

umask 077

# If a non-interactive shell sources this file, do not do anything.
if [[ "${-}" != *i* ]]; then
    return
fi

unset LS_COLORS PROMPT_COMMAND
unalias -a

# If a variable changes between shell invocations, or not required in child
# shells or forked programs, or makes sense only in an interactive shell, it
# belongs in the shell run-control file (e.g. ~/.bashrc).  These variables are
# typically used by the shell itself or shell builtins, so exporting them is
# unnecessary.
GLOBIGNORE='.:..'
HISTCONTROL='ignoreboth'
HISTSIZE=10000
HISTFILESIZE=20000
HISTTIMEFORMAT='%F %a %T %Z '
PROMPT_DIRTRIM=3
PS1='(\H)\w\$ '

shopt -u -o posix vi
shopt -s -o emacs pipefail
shopt -s checkhash checkwinsize cmdhist dotglob extglob histappend \
         histreedit histverify no_empty_cmd_completion sourcepath

ls() {
    command ls -abF "${@}"
}

grep() {
    command grep --color=auto "${@}"
}

cgrep() {
    command grep --color=always "${@}"
}

t() {
    local py_cmd=
    if [[ -z "${1}" ]]; then
        py_cmd='import time; print time.strftime("%s %F %a %T %Z(%z)");'
    elif [[ "${1}" =~ ^(\+|\-)?[0-9]+(\.[0-9]+)?$ ]]; then
        py_cmd='import time; print time.strftime("%s %F %a %T %Z(%z)", time.localtime('"${1}"'));'
    fi
    if [[ -n "${py_cmd}" ]]; then
        python -c "${py_cmd}"
    else
        return 1
    fi
}
