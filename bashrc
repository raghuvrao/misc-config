# ~/.bashrc
# Raghu Rao

[[ ${-} == *i* ]] || return

shopt -u -o posix
shopt -u -o vi
shopt -s -o emacs
shopt -s -o pipefail
shopt -s extglob
shopt -s globstar
shopt -s histverify
shopt -s histappend
shopt -s checkwinsize
shopt -s no_empty_cmd_completion

export HISTCONTROL='ignoreboth'
export HISTSIZE=10000
export HISTFILESIZE=20000
export HISTTIMEFORMAT='%F %T '

# On Debian, bash-completion is sourced from /etc/profile (via sourcing
# /etc/profile.d/bash_completion.sh).  So, either start the shell as a
# login shell or uncomment the following if you want bash-completion.
# Source this file early on (e.g. before setting your PS1, if you want
# to use __git_ps1 and such other functions in your prompt).
if [[ -f /etc/bash_completion ]]; then
    . /etc/bash_completion
fi

unset PROMPT_COMMAND
t="$(type -t __git_ps1)"
if [[ ${t} == function ]]; then
    PROMPT_COMMAND='__git_ps1 "[%s]\n"'
    export GIT_PS1_SHOWDIRTYSTATE='yes'
    export GIT_PS1_SHOWUNTRACKEDFILES='yes'
fi
unset t

unset LS_COLORS
prompt='\h ${?} \w\$ '
if c="$(tput setaf 2 2>/dev/null)" && r="$(tput sgr0 2>/dev/null)"; then
    PS1="\[${c}\]${prompt}\[${r}\]"
    if type -P dircolors >/dev/null 2>&1; then
        if [[ -f "${HOME}"/.dir_colors ]]; then
            eval "$(dircolors -b "${HOME}"/.dir_colors)"
        elif [[ -f /etc/DIR_COLORS ]]; then
            eval "$(dircolors -b /etc/DIR_COLORS)"
        else
            eval "$(dircolors -b)"
        fi
    fi
else
    PS1="${prompt}"
fi
unset c r prompt

case "${TERM}" in
    rxvt*|xterm*)
        # xterm title
        PS1="\[\e]0;\h \w\a\]${PS1}"
    ;;
esac

unalias -a
l() { command ls --color=always "$@"; }

if [[ -r "${HOME}"/.bash_office ]]; then
   . "${HOME}"/.bash_office
fi
