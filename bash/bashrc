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

# Make pipeline's return status the value of the last (rightmost) command to
# exit with a non-zero status, or zero if all commands exit successfully.
shopt -o -s pipefail

# Delete/modify/add aliases.
for a in ls ll l. vi; do
  if alias "${a}" &>/dev/null; then unalias "${a}"; fi
done
unset -v a
if ! type -a ll &>/dev/null; then alias ll='ls -l -a'; fi
if ! type -a acgrep &>/dev/null; then alias acgrep='grep --color=always'; fi
if ! type -a e &>/dev/null; then alias e='emacsclient -c -t'; fi

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
    unset -v first_line
  fi
  unset -v f
fi
