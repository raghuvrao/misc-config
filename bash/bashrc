# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>

umask 077

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

p='/etc/bashrc'
if [[ -f "${p}" && -r "${p}" ]]; then
  source "${p}"
fi
unset -v p

HISTCONTROL='ignoreboth'
HISTFILESIZE=20000
HISTSIZE="${HISTFILESIZE}"
HISTTIMEFORMAT='%F %a %T %Z(UTC%z) '

# Make sure bash updates its idea of window size after each command.
shopt -s checkwinsize

# Remove some aliases that I dislike.
for a in ls ll l. vi; do
  if alias "${a}" &>/dev/null; then unalias "${a}"; fi
done

# Handy function to force color sequences in grep's output.  Useful when piping
# to 'less -R'.
if ! type -a acgrep &>/dev/null; then
  acgrep() {
    command grep --color=always "${@}"
  }
fi

# Make ll a bit more useful.
ll() {
  if [[ -n ${INSIDE_EMACS+x} ]]; then
    command ls -lA "${@}"
  else
    command ls -lA "${@}" | less -F -X
  fi
}

# In Slackware, when running bash, readline's clear-screen function (bound to
# C-l by default) does not seem to work as expected for certain TERMs (e.g.
# xterm-256color, screen-256color and screen).  For these TERMs, the command
# 'tput clear' works as expected.  So, work around the problem by rebinding
# C-l to 'tput clear' until I find a proper solution.
if [[ "${TERM}" =~ xterm-.*|screen.* ]]; then
  f="/etc/slackware-version"
  if [[ -r "${f}" ]]; then
    read -r first_line <"${f}" &>/dev/null
    if [[ "${first_line}" =~ ^[Ss]lackware ]]; then
      builtin bind -r "\C-l"
      builtin bind -x '"\C-l": tput clear'
    fi
  fi
  unset -v f
fi
