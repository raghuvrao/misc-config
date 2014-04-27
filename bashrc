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

GLOBIGNORE=".:.."
HISTCONTROL='ignoreboth'
HISTSIZE=10000
HISTFILESIZE=20000
HISTTIMEFORMAT='%F %T '
PROMPT_DIRTRIM=3
PS1='[\h ${?} \w]\$ '

ls() { command ls -A -F "${@}"; }
if type -P dircolors >/dev/null 2>&1; then
    unset ls
    ls() { command ls -A --color=auto "${@}"; }
    eval "$(dircolors -b)"
fi

[[ -f /etc/bash_completion.d/git ]] && {
    source /etc/bash_completion.d/git
    GIT_PS1_SHOWDIRTYSTATE='yes'
    GIT_PS1_SHOWUNTRACKEDFILES='yes'
    GIT_PS1_SHOWSTASHSTATE='yes'
    PS1='[\h ${?} \w$(__git_ps1 " (%s)")]\$ '
}
