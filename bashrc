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
case "${TERM}" in
    xterm*|screen*)
        if [[ -z "${PROMPT_COMMAND}" ]]; then
            if [[ "${TERM}" == xterm* ]]; then
                PROMPT_COMMAND='printf '"'\033]2;%s\007'"' "${HOSTNAME}"'
            elif [[ "${TERM}" == screen* ]]; then  # For tmux, really.
                # In tmux, the sequence in t1 below sets the pane_title
                # variable.  The sequence in t2 sets the window_name variable.
                # I use both these variables in my tmux configuration for the
                # window title, so use both sequences here.  Thankfully, GNU
                # screen does not seem to get upset when I use both sequences,
                # so additional checks are not necessary.  Even more
                # thankfully, when I start tmux on the Linux console (where
                # window-title-setting is irrelevant because there are no
                # windows), tmux will not let the sequences below do funny
                # things.
                t1='\033]2;%s\033\134'
                t2='\033k%s\033\134'
                PROMPT_COMMAND='printf '"'${t1}${t2}'"' "${HOSTNAME}" "${HOSTNAME}"'
                unset t1 t2
            else
                PROMPT_COMMAND=
            fi
        fi
        # For reasons I do not yet understand, C-l (by default, bound to
        # readline's clear-screen function) does not clear the screen in bash
        # on Slackware for some terminal types. E.g. C-l does not work as
        # expected when TERM is screen, screen-256color or xterm-256color
        # (possibly others), but works just fine when TERM is xterm or linux.
        # On the terminal types where C-l does not work, the command 'tput
        # clear' works.  So, work around the problem for now by binding C-l to
        # 'tput clear'.
        if [[ "${TERM}" =~ xterm-.*|screen.* ]]; then
            f="/etc/slackware-version"
            if [[ -r "${f}" && "${TERM}" =~ xterm\-.*|screen.* ]]; then
                read -r first_line <"${f}" &>/dev/null
                if [[ "${first_line}" =~ ^[Ss]lackware ]]; then
                    builtin bind -r "\C-l"
                    builtin bind -x '"\C-l": tput clear'
                fi
            fi
            unset f
        fi
        ;;
esac

unset -v p
