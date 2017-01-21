# ~/.bashrc
# Author: Raghu Rao <raghu.v.rao@gmail.com>

umask 077

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

p='/etc/bashrc'
if [[ -f "${p}" && -r "${p}" ]]; then
    source "${p}"
fi

HISTCONTROL='ignoreboth'
HISTFILESIZE=20000
HISTSIZE="${HISTFILESIZE}"
HISTTIMEFORMAT='%F %a %T %Z(UTC%z) '

# Set up a handy alias to force grep to color my output (useful when piping
# grep output through less -R).
if ! type -a cgrep &>/dev/null; then alias cgrep='grep --color=always'; fi

# Make sure bash updates its idea of window size after each command.
shopt -s checkwinsize

# Do terminal-type-specific stuff (setting window-title sequences, rebinding
# shortcuts etc.).
#
# Read the 'C1 (8-Bit) Control Characters' and 'Operating System Commands'
# sections at < http://invisible-island.net/xterm/ctlseqs/ctlseqs.html >.
#
#   Operating System Command (OSC) is the following sequence:
#
#       ESC ]
#
#   OSC 0 ; TXT BEL -> set terminal emulator window title and icon name (tab
#                      title in some terminal emulators, e.g. iTerm2) to TXT
#   OSC 1 ; TXT BEL -> set only icon name (tab title in some terminal
#                      emulators) to TXT
#   OSC 2 ; TXT BEL -> set only terminal emulator window title to TXT
#
# The first sequence above is probably the least headache.  It also sets
# pane_title in tmux and the hardstatus in GNU screen.  Therefore, use that
# sequence in s1 below.
#
# The second sequence (s2 below) seems specific to tmux and GNU screen, and it
# sets the screen-/tmux-specific window name (not the terminal emulator window
# title bar text).  In tmux, this sequence sets the window_name variable.
# tmux can be configured to include window_name in the terminal emulator title
# bar text (which is what I have done in my ~/.tmux.conf), so I will use s2
# along with s1 when in tmux (TERM=screen*).  I have configured GNU screen to
# set the terminal window title bar text to the hardstatus, so s1 takes care
# of title-setting when in GNU screen.  Having the same TXT portion in both s1
# and s2 ensures I get the same string in both the emulator's title bar text
# and the tmux/screen window name.
#
# Fortunately, neither s1 nor s2 seem to do any harm when running tmux/screen
# at the Linux console (e.g. TERM=screen.linux).
unset PROMPT_COMMAND
s1='\e]0;%s(%s)\a'
s2='\ek%s(%s)\e\\'
if [[ "${TERM}" == xterm* ]]; then
    PROMPT_COMMAND='printf '"'${s1}' "'"${HOSTNAME}" "${$}"'
elif [[ "${TERM}" == screen* ]]; then
    PROMPT_COMMAND='printf '"'${s1}${s2}'"' "${HOSTNAME}" "${$}" "${HOSTNAME}" "${$}"'
fi
unset s1 s2

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
    unset f
fi

unset -v p
