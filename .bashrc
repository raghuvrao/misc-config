# ~/.bashrc
# Raghu Rao

# If shell is not interactive, simply return.
# Do not do anything else.
if [[ ! "${-}" =~ \*i\* ]]; then
    return 0
fi

# If a pipeline fails, get the exit code of the right-most command that
# failed instead of the final command's exit code.
shopt -s -o pipefail

# Emacs-like keybindings at the shell prompt are better than vi-like
# bindings for me.
shopt -s -o emacs
shopt -u -o vi

# I like to be able to use process substitution.
shopt -u -o posix

# Some useful globbing options.
shopt -s extglob
shopt -s failglob
shopt -s globstar

# Misc. useful options.
shopt -s histverify
shopt -s histappend
shopt -s checkwinsize
shopt -s no_empty_cmd_completion

export HISTCONTROL='ignoreboth'
export HISTSIZE=1000
export HISTFILESIZE=2000
export HISTTIMEFORMAT='%F %T '

PS1='\h ${?} \w\$ '

# Not going to bother with xterm title within screen/tmux.
unset PROMPT_COMMAND
case "${TERM}" in
rxvt*|xterm*)
    PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME%%.*} ${PWD/#$HOME/~}\007"'
;;
esac

# Add custom colorization for ls if available.
unset LS_COLORS
if [[ -x /usr/bin/dircolors ]]; then 
    if [[ -f ${HOME}/.dir_colors ]]; then
        eval "$(/usr/bin/dircolors -b ${HOME}/.dir_colors)"
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

# Work-related stuff.
if [[ -r ${HOME}/.bash_office ]]; then
   . ${HOME}/.bash_office
fi
