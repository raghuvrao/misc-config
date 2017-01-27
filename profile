# ~/.profile
# Author: Raghu Rao <raghu.v.rao@gmail.com>

# This file must be fully sh-compatible as it is sourced by multiple sh-like
# shells!  No bashisms!

# Stolen from Red Hat's /etc/profile
pathmunge() {
    case ":${PATH}:" in
        *:"${1}":*)
            ;;
        *)
            if [ "${2}" = "after" ]; then
                PATH="${PATH}:${1}"
            else
                PATH="${1}:${PATH}"
            fi
            ;;
    esac
}

# Some general guidelines to tell if an environment variable belongs in this
# file:
#   - its value remains the same in all shell invocations
#   - it is expected to be available to child processes (e.g. programs other
#     than the shell use it)
#   - it does not concern interactive shell sessions in particular (e.g. PS1 is
#     interactive-only, so it does not belong in this file)
#   - it is not bash-specific
# In order that these variables be available to child processes, it is
# necessary to export them.

export VISUAL='vim'
export EDITOR="${VISUAL}"
export FCEDIT="${VISUAL}"
export SVN_EDITOR="${VISUAL}"
export PAGER='less'
export LESS='-i -R'

if [ -d "${HOME}/lib/python" ]; then
    export PYTHONPATH="${PYTHONPATH}:${HOME}/lib/python"
fi

if [ -r "${HOME}/.pythonrc.py" ]; then
    export PYTHONSTARTUP="${HOME}/.pythonrc.py"
fi

if [ -d "${HOME}/bin" ]; then
    pathmunge "${HOME}/bin" "after"
fi

GOROOT="${HOME}/golang/root"
if [ -d "${GOROOT}" ]; then
    export GOROOT
    if [ -d "${GOROOT}/bin" ]; then
        pathmunge "${GOROOT}/bin" "after"
    fi
else
    unset GOROOT
fi

GOPATH="${HOME}/go"
if [ -d "${GOPATH}" ]; then
    export GOPATH
    if [ -d "${GOPATH}/bin" ]; then
        pathmunge "${GOPATH}/bin" "after"
    fi
else
    unset GOPATH
fi

export PATH
unset -f pathmunge

# Source .bashrc in the end, and only if running bash.
if [ -n "${BASH_VERSION}" -a -r "${HOME}/.bashrc" ]; then
    . "${HOME}/.bashrc"
fi
