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

unset PROMPT_COMMAND

color="$(tput setaf 6)"
reset="$(tput sgr0)"
color_prompt="\[${color}\](\h)\[${reset}\]"' ${?} \w\$ '
plain_prompt='(\h) ${?} \w\$ '
case "${TERM}" in
    rxvt*|xterm*)
        # xterm title
        PS1="\[\e]0;\h \w\a\]${color_prompt}"
    ;;
    linux*|screen*)
        # Not going to bother with xterm title within screen/tmux.
        PS1="${color_prompt}"
    ;;
    *)
        PS1="${plain_prompt}"
    ;;
esac
unset color reset color_prompt plain_prompt

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

# On Debian, bash-completion is sourced from /etc/profile (via sourcing
# /etc/profile.d/bash_completion.sh).  So, either start the shell as a
# login shell or uncomment the following if you want bash-completion.
if [[ -f /etc/bash_completion ]]; then
    . /etc/bash_completion
fi

if [[ -r "${HOME}"/.bash_office ]]; then
   . "${HOME}"/.bash_office
fi
