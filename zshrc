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
bindkey -M emacs '\ef' emacs-forward-word

# While I prefer bash's default way of traversing/deleting words, sometimes,
# I want to use zsh's default way of traversing/deleting words too.  The
# difference is that zsh's default set of word separators is far smaller than
# bash's.  The following few my_* functions and widgets provide zsh's default
# behaviours at non-default bindings.

my_backward_word () {
	local WORDCHARS=''
	unset -v WORDCHARS
	zle backward-word
}
zle -N my-backward-word my_backward_word
bindkey -M emacs '\eB' my-backward-word

my_forward_word () {
	local WORDCHARS=''
	unset -v WORDCHARS
	zle forward-word
}
zle -N my-forward-word my_forward_word
bindkey -M emacs '\eF' my-forward-word

my_backward_kill_word_from_point () {
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

# Setting WORDCHARS to the empty value is NOT the same as unsetting WORDCHARS.
# The effect of setting WORDCHARS to the empty string is that zle will treat
# many more so-called special characters as word separators than its limited
# default set (which seems to be space, comma and colon).
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
