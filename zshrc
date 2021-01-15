#!/bin/zsh

# ~/.zshrc
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

if [[ ! -o INTERACTIVE ]]; then
    return
fi

bindkey -e

bindkey -M emacs '\ef' emacs-forward-word
bindkey -M emacs '^U' backward-kill-line

_default_backward_kill_word () {
    local WORDCHARS=''
    unset -v WORDCHARS
    zle .backward-kill-word
}
zle -N _default_backward_kill_word
bindkey -M emacs '^W' _default_backward_kill_word

setopt COMBINING_CHARS
setopt EXTENDED_HISTORY
setopt GLOB_DOTS
setopt HIST_IGNORE_DUPS
setopt INTERACTIVE_COMMENTS
setopt LIST_PACKED
setopt MULTIBYTE

unsetopt ALWAYS_LAST_PROMPT
unsetopt LIST_BEEP
unsetopt PROMPT_CR
unsetopt PROMPT_SP

HISTFILE="${HOME}/.zsh_history"
HISTSIZE=10500
SAVEHIST=10000

WORDCHARS=''

PS1='(%M %(0?.%?.%B%?%b) %3~) %# '

# When I type '&' or '|' immediately after the completion system has
# inserted a completion and a suffix character, do not remove the suffix
# character.  See 'man zshparam' for more information about
# ZLE_REMOVE_SUFFIX_CHARS.
ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;'

# See the file SETTING-TITLES for information about setting terminal
# emulator window/tab titles, and icon names.
case "${TERM}" in
    (xterm*|rxvt*)
        precmd () {
            print -Pn '\e]0;%M:%3~\a'
        }
        ;;
    (tmux*|screen*)
        precmd () {
            print -Pn '\e]0;%M:%3~\a\ek%M:%3~\e\\'
        }
        ;;
esac

_with_max_manwidth_80 () {
    if [[ ${#} -lt 1 ]]; then
        return 1
    fi
    local cmd="${1}"
    shift
    if [[ -n "${MANWIDTH}" || -z "${COLUMNS}" ]]; then
        command "${cmd}" "${@}"
    else
        if [[ ${COLUMNS} -ge 80 ]]; then
            MANWIDTH=80 command "${cmd}" "${@}"
        elif [[ ${COLUMNS} -ge 66 ]]; then
            # Some versions of 'man' use 80 columns if COLUMNS (or
            # MANWIDTH) is in the range 66-80, which makes the manual
            # page display look bad in windows with 66-79 columns.  In
            # these situations, tell 'man' that there are only 65
            # columns.
            MANWIDTH=65 command "${cmd}" "${@}"
        else
            command "${cmd}" "${@}"
        fi
    fi
}

alias cgrep='command grep --color=always'
alias git='_with_max_manwidth_80 git'
alias grep='grep --color=auto'
alias info='_with_max_manwidth_80 info'
alias ll='ls -l'
alias ls='ls -A -b'
alias man='_with_max_manwidth_80 man'
alias which-command='whence -a -v'
