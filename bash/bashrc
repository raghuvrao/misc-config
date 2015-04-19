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

PROMPT_DIRTRIM=3

ls() { command ls -AbF "${@}"; }
grep() { command grep --color=auto "${@}"; }

if co="$(tput setaf 1 2>/dev/null)"; then
    rst="$(tput sgr0 2>/dev/null)"
    ps1_deco="\[${rst}${co}\]»\[${rst}\]"
    unset co rst
else
    ps1_deco='\n'
fi
PS1="${ps1_deco}"'[\w \h]\$ '
unset ps1_deco
