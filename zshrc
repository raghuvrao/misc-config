#!/bin/zsh

if [[ ! -o INTERACTIVE ]]; then return; fi

# Slackware likes to set MANPATH even though /etc/man_db.conf is configured
# satisfactorily (so far as I go).  Slackware's default MANPATH causes
# problems sometimes.  E.g. when two versions of a program are installed in
# two different locations, sometimes man pulls up the version of the man page
# that does not match the version of the command.  Unsetting MANPATH seems to
# solve this problem.
unset -v MANPATH

# Use Emacs-style key-bindings regardless of the value of EDITOR.
bindkey -e

# By default, TAB is bound to the ZLE function expand-or-complete.
# expand-or-complete attempts to complete the word rather than the prefix, so
# it takes into account the characters both before and after the cursor.
# I like for completion to work only on the letters before the cursor, which
# is what the ZLE function expand-or-complete-prefix does.
bindkey -M emacs '\t' expand-or-complete-prefix

# By default, C-U is bound to the ZLE function kill-whole-line.
# kill-whole-line, as the name suggests, kills the entire line regardless of
# the cursor's position.  I vastly prefer C-U to kill from the cursor's
# position to the beginning of the line, like in bash.
bindkey -M emacs '^U' backward-kill-line

# Make zsh treat more characters as word separators.
#
# The default value for WORDCHARS is *?_-.[]~=/&;!#$%^(){}<> which means ZLE
# will treat none of these characters as a word separator.  Also, unsetting
# WORDCHARS is not equivalent to setting it to the empty value.  If WORDCHARS
# is unset, ZLE will use its (aggressive) default value.
WORDCHARS=''

# Be default, M-f is bound to forward-word, which moves point to the beginning
# of the next word (subject, of course, to WORDCHARS).  In Emacs and bash, M-f
# moves point forward to the end of the word, which I find more convenient.
# Fortunately, ZLE provides the function emacs-forward-word, which provides
# the behaviour I prefer.
bindkey -M emacs '\ef' emacs-forward-word

# Create a few functions, and corresponding ZLE widgets and key-bindings to
# keep zsh's default concept of words handy.
#
# By creating a local version of WORDCHARS and unsetting it, we can pretend
# that WORDCHARS does not exist in the environment (see the documentation for
# the 'unset' builtin in 'man zshbuiltins').  Unsetting WORDCHARS is not the
# same as setting WORDCHARS to the empty value.

my_emacs_backward_word () {
	local WORDCHARS=''
	unset -v WORDCHARS
	zle emacs-backward-word
}
zle -N my-emacs-backward-word my_emacs_backward_word
bindkey -M emacs '\eB' my-emacs-backward-word

my_emacs_forward_word () {
	local WORDCHARS=''
	unset -v WORDCHARS
	zle emacs-forward-word
}
zle -N my-emacs-forward-word my_emacs_forward_word
bindkey -M emacs '\eF' my-emacs-forward-word

my_backward_kill_word () {
	local WORDCHARS=''
	unset -v WORDCHARS
	zle backward-kill-word
}
zle -N my_backward_kill_word my_backward_kill_word
bindkey -M emacs '^W' my_backward_kill_word

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
