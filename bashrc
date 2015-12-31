# ~/.bashrc
# Raghu Rao

umask 077

if [[ ${-} != *i* ]]; then
    return
fi

unalias -a
unset PROMPT_COMMAND
unset LS_COLORS

export LESS='-R -i'
export VISUAL='vim'
export EDITOR="${VISUAL}"
export FCEDIT="${VISUAL}"

HISTCONTROL='ignoreboth'
HISTSIZE=10000
HISTFILESIZE=20000
HISTTIMEFORMAT='%F %a %T %Z '
GLOBIGNORE='.:..'
PROMPT_DIRTRIM=3
PS1='(\h)\w\$ '

shopt -u -o posix
shopt -u -o vi
shopt -s -o emacs
shopt -s -o pipefail
shopt -s extglob
shopt -s dotglob
shopt -s globstar
shopt -s histverify
shopt -s histappend
shopt -s checkwinsize
shopt -s no_empty_cmd_completion

ls() {
    command ls -AbF "${@}"
}

grep() {
    command grep --color=auto "${@}"
}

cgrep() {
    command grep --color=always "${@}"
}

if [[ -d "${HOME}/lib/python" ]]; then
    export PYTHONPATH="${PYTHONPATH}:${HOME}/lib/python"
fi

if [[ -f "${HOME}/.pythonrc.py" ]]; then
    export PYTHONSTARTUP="${HOME}/.pythonrc.py"
fi

if [[ -d "${HOME}/bin" ]]; then
    PATH+=":${HOME}/bin"
fi
