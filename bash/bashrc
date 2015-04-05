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
PS1='[\w \h]\$ '

ls() { command ls -AbF "${@}"; }
grep() { command grep --color=auto "${@}"; }
