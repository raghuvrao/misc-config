# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>
#
# Unlike .profile, this file is for use only with bash, so bashisms (e.g.
# ${HOSTNAME} and [[ ... ]]) may be used in this file.

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

# Make sure bash updates its idea of window size after each command.
shopt -s checkwinsize

# Make pipeline's return status the value of the last (rightmost) command to
# exit with a non-zero status, or zero if all commands exit successfully.
shopt -o -s pipefail

# Do not perform completion when completion is attempted on an empty line.
shopt -s no_empty_cmd_completion

# Do not save lines matching previous history entry.
HISTCONTROL='ignoredups'

# Delete/modify/add aliases.
for a in ls ll l. vi; do
  if alias "${a}" &>/dev/null; then unalias "${a}"; fi
done
unset -v a
if ! type -a ll &>/dev/null; then alias ll='ls -l -a'; fi
if ! type -a acgrep &>/dev/null; then alias acgrep='grep --color=always'; fi

# Set up a simple shell prompt.
PS1='\h:\w\$ '
PROMPT_DIRTRIM=3

# Use PROMPT_COMMAND to set terminal emulator window/tab titles and icon names.
# See the accompanying file SETTING-TITLES for a brief discussion.  Below,
# I have escaped `~' as recent versions of bash require it (without the escape,
# `~' is expanded to the home directory before use, making the substitution
# pointless).  In older versions of bash, `~' should not be escaped as it can
# cause the `\' to appear in the output.
unset -v PROMPT_COMMAND
s='\033]0;%s:%s\007'
h="${HOSTNAME%%.*}"
case "${TERM}" in
  xterm*)
    PROMPT_COMMAND="printf '${s}' '${h}'"' "${PWD/#${HOME}/\~}"'
    ;;
  screen*)
    s+='\033k%s:%s\033\134'
    PROMPT_COMMAND="printf '${s}' '${h}'"' "${PWD/#${HOME}/\~}"'" '${h}'"' "${PWD/#${HOME}/\~}"'
    ;;
esac
unset -v h s

# In Slackware, when running bash, readline's clear-screen function (bound to
# C-l by default) does not seem to work as expected for certain TERMs (e.g.
# xterm-256color, screen-256color and screen).  For these TERMs, the command
# 'tput clear' works as expected.  So, work around the problem by rebinding
# C-l to 'tput clear' until I find a proper solution.
if [[ "${TERM}" =~ xterm-.*|screen.*|rxvt.* ]]; then
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
