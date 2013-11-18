# ~/.bashrc
# Raghu Rao

[[ "${-}" != *i* ]] && return

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
export HISTSIZE=1000
export HISTFILESIZE=2000
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
if [[ "$(type -t __git_ps1)" == "function" ]]; then
    PROMPT_COMMAND='__git_ps1 "[%s]\n"'
fi
export GIT_PS1_SHOWDIRTYSTATE='yes'
export GIT_PS1_SHOWUNTRACKEDFILES='yes'

color="$(tput setaf 2)"
reset="$(tput sgr0)"
prompt='\h ${?} \w\$ '
if [[ -n "${color}" ]]; then
    PS1="\[${color}\]${prompt}\[${reset}\]"
else
    PS1="${prompt}"
fi
case "${TERM}" in
    rxvt*|xterm*)
        # xterm title
        PS1="\[\e]0;\h \w\a\]${PS1}"
    ;;
esac
unset color reset prompt

unset LS_COLORS
if [[ -x /usr/bin/dircolors ]]; then 
    if [[ -f "${HOME}"/.dir_colors ]]; then
        eval "$(/usr/bin/dircolors -b "${HOME}"/.dir_colors)"
    else
        eval "$(/usr/bin/dircolors -b)"
    fi
fi

unalias -a
alias l='ls --color=tty'

if [[ -r "${HOME}"/.bash_office ]]; then
   . "${HOME}"/.bash_office
fi
