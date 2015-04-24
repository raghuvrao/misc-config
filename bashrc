# ~/.bashrc
# Raghu Rao

umask 077

[[ ${-} == *i* ]] || return

unalias -a
unset PROMPT_COMMAND
unset LS_COLORS

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

ls() { command ls -AbF "${@}"; }
grep() { command grep --color=auto "${@}"; }
cgrep() { command grep --color=always "${@}"; }

PROMPT_DIRTRIM=3

_ps1='[\w \h]\$ '
PS1='\n'"${_ps1}"
if co="$(tput setaf 1 2>/dev/null)"; then
    rst="$(tput sgr0 2>/dev/null)"
    PS1="\[${rst}${co}\]§\[${rst}\]${_ps1}"
    unset co rst
fi
unset _ps1
