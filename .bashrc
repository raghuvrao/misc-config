# ~/.bashrc
# Raghu Rao

# posix - unset to allow use of process substitution
# extglob - set to allow use of extended globbing
shopt -u -o posix
shopt -s extglob

if [[ "${-}" != *i* ]]; then
    return 0
fi

shopt -u -o vi
shopt -s -o emacs
shopt -s -o pipefail
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

color="$(tput setaf 6)"
reset="$(tput sgr0)"
prompt_common=' ${?} \w\$ '
if [[ -n "${color}" ]]; then
    prompt="\[${color}\](\h)\[${reset}\]${prompt_common}"
else
    prompt="(\h)${prompt_common}"
fi
case "${TERM}" in
    rxvt*|xterm*)
        # xterm title
        PS1="\[\e]0;\h \w\a\]${prompt}"
    ;;
    *)
        # Not going to bother with xterm title within screen/tmux.
        PS1="${prompt}"
    ;;
esac
unset color reset prompt prompt_common

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
