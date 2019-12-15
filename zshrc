#!/bin/zsh

if [[ ! -o INTERACTIVE ]]; then return; fi

# Slackware likes to set MANPATH even though /etc/man_db.conf is configured
# satisfactorily (so far as I go).  Slackware's default MANPATH causes
# problems sometimes.  E.g. when two versions of a program are installed in
# two different locations, sometimes man pulls up the version of the man page
# that does not match the version of the command.  Unsetting MANPATH seems to
# solve this problem.
unset -v MANPATH

bindkey -e

bindkey -M emacs '\t' expand-or-complete-prefix
bindkey -M emacs '\eb' emacs-backward-word
bindkey -M emacs '\eB' emacs-backward-word
bindkey -M emacs '\ef' emacs-forward-word
bindkey -M emacs '\eF' emacs-forward-word

my_backward_kill_word_from_point () {
	# By creating a local version of WORDCHARS, and unsetting it, zle in
	# this local scope will operate without WORDCHARS being set.  When
	# WORDCHARS is unset, zle uses space (and perhaps comma) as the word
	# separator, and not any of the other special characters.  Ultimately,
	# I get an easy means to delete from point to the previous space.
	local WORDCHARS=''
	unset -v WORDCHARS
	zle backward-kill-word
}
zle -N my_backward_kill_word_from_point my_backward_kill_word_from_point
bindkey -M emacs '^W' my_backward_kill_word_from_point

setopt APPEND_HISTORY
setopt AUTO_MENU
setopt COMBINING_CHARS
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_DUPS
setopt INTERACTIVE_COMMENTS
setopt LIST_PACKED
setopt MULTIBYTE

unsetopt LIST_BEEP
unsetopt LIST_TYPES
unsetopt MENU_COMPLETE
unsetopt PROMPT_CR
unsetopt PROMPT_SP

HISTFILE="${HOME}/.zsh_history"
HISTSIZE=10500
SAVEHIST=10000

# Not the same as WORDCHARS being unset.
WORDCHARS=''

PS1='[%M %? %3~] %# '

autoload -Uz edit-command-line
zle -N edit-command-line edit-command-line
bindkey -M emacs '^X^E' edit-command-line

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
