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

# Quick git summary, if ${PWD} is a git repository.
_git_summary()
{
    [[ -d ./.git ]] || return

    _marker=""
    _git_status="$(git status -unormal 2>&1)"
    _r="Not a git repo"
    if [[ "${_git_status}" =~ ${_r} ]]; then
        return
    else
        _git_branch="$(git branch | awk '/^\*/ { print $2 }')"
        _r="working directory clean"
        if [[ ${_git_status} =~ ${_r} ]]; then
            _marker="C"
        fi
        _r="new file:"
        if [[ ${_git_status} =~ ${_r} ]]; then
            _marker+="N"
        fi
        _r="modified:"
        if [[ ${_git_status} =~ ${_r} ]]; then
            _marker+="M"
        fi
        _r="Untracked files"
        if [[ ${_git_status} =~ ${_r} ]]; then
            _marker+="U"
        fi
        _r="Your branch is ahead"
        if [[ ${_git_status} =~ ${r} ]]; then
            _marker+="+"
        fi
    fi
    printf "${_marker} ${_git_branch}\n"
    unset _marker _git_branch _r _git_status
}
PROMPT_COMMAND=_git_summary

plain_prompt='\h ${?} \w\$ '
color="$(tput setaf 6)"
reset="$(tput sgr0)"
case "${TERM}" in
    rxvt*|xterm*)
        PS1="\[""${color}""\]""${plain_prompt}""\[""${reset}""\]"
        # xterm title
        PS1="\[\e]0;\h \w\a\]""${PS1}"
    ;;
    screen*)
        # Not going to bother with xterm title within screen/tmux.
        PS1="\[""${color}""\]""${plain_prompt}""\[""${reset}""\]"
    ;;
    *)
        PS1="${plain_prompt}"
    ;;
esac
unset color reset plain_prompt

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
