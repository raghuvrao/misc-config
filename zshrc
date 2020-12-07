#!/bin/zsh

if [[ ! -o INTERACTIVE ]]; then return; fi

# Slackware's default MANPATH (set in /etc/{z,}profile) causes problems
# sometimes.  E.g. when two versions of a program are installed in two
# different locations, sometimes 'man' displays the version of the
# manual page that does not match the version of the command found first
# in PATH. Unsetting MANPATH seems to solve this problem.  So far as
# I go, /etc/man_db.conf in Slackware is configured satisfactorily.
#
# zsh handles MANPATH (and the corresponding 'manpath' array) in
# a strange way.  In 'man zshparam', MANPATH (and manpath) are listed
# under 'PARAMETERS USED BY THE SHELL' and not under 'PARAMETERS SET BY
# THE SHELL'.  However, if MANPATH does not exist in the environment,
# and if I am not unsetting MANPATH myself, zsh creates the MANPATH
# variable itself, set to the empty value.  So, the documentation does
# not seem to match actual behaviour.  Also, I find it strange and
# unnecessary that the shell meddles with MANPATH.
#
# Unsetting MANPATH in ~/.zshrc (vs. in ~/.zprofile) will ensure MANPATH
# is fully removed from the environment: non-login zsh processes
# (interactive or otherwise) will not read ~/.zprofile, so they will not
# know to remove MANPATH.  zsh will ensure MANPATH and the array manpath
# are kept synchronized.
unset -v MANPATH

bindkey -v

bindkey -M vicmd 'K' run-help
bindkey -M viins '^_' insert-last-word
bindkey -M viins '^T' history-incremental-search-backward
bindkey -M viins '^Y' accept-and-hold
bindkey -M vicmd '^Y' accept-and-hold
bindkey -M viins '^Z' which-command

setopt COMBINING_CHARS
setopt EXTENDED_HISTORY
setopt GLOB_DOTS
setopt HIST_IGNORE_DUPS
setopt INTERACTIVE_COMMENTS
setopt LIST_PACKED
setopt MULTIBYTE

unsetopt ALWAYS_LAST_PROMPT
unsetopt LIST_BEEP
unsetopt LIST_TYPES
unsetopt PROMPT_CR
unsetopt PROMPT_SP

# When I type '&' or '|' immediately after the completion system has
# inserted a completion and a suffix character, do not remove the suffix
# character.  See 'man zshparam' for more information about
# ZLE_REMOVE_SUFFIX_CHARS.
ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;'

HISTFILE="${HOME}/.zsh_history"
HISTSIZE=10500
SAVEHIST=10000

PS1='[%M %? %3~] %# '

case "${TERM}" in
    (xterm*|rxvt*)
        precmd () { print -Pn '\e]0;%M:%~\a'; }
        ;;
    (tmux*|screen*)
        precmd () { print -Pn '\e]0;%M:%~\a\ek%M:%~\e\\'; }
        ;;
esac

alias ls='ls -A -b'
alias ll='ls -l'
alias grep='grep --color=auto'
alias cgrep='command grep --color=always'
alias which-command='whence -a -v'
