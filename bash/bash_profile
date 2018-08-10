#!/bin/sh

# .bash_profile
# Author: Raghu Rao <raghu.v.rao@gmail.com>

# dir_r_x returns true if its first argument is a readable directory into which
# we can descend; false otherwise.
dir_r_x() {
    test -d "${1}" -a -r "${1}" -a -x "${1}"
    return ${?}
}

# pathmunge() appends its first argument to PATH if the second argument is the
# string `after'; otherwise, pathmunge() prefixes its first argument to PATH.
# The first argument must be a readable directory into which we can descend;
# otherwise, pathmunge() does nothing to PATH, regardless of the second
# argument.  This version of pathmunge() is a modified version of pathmunge()
# from Red Hat's /etc/profile.
pathmunge() {
    # Include only directories that we can both read and into which we can
    # descend.
    if dir_r_x "${1}"; then
        case ":${PATH}:" in
            ::)
                # PATH is empty; avoid leading/trailing colon.
                PATH="${1}"
                ;;
            *:"${1}":*)
                # PATH already has ${1} in it; do nothing.
                ;;
            *)
                if [ "${2}" = "after" ]; then
                    PATH="${PATH}:${1}"
                else
                    PATH="${1}:${PATH}"
                fi
                ;;
        esac
    fi
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

export EDITOR='vim'
export FCEDIT="${EDITOR}"
export GIT_EDITOR="${EDITOR}"
export SVN_EDITOR="${EDITOR}"
export VISUAL="${EDITOR}"

export GIT_PAGER='less -+F -X'
export PAGER='less'

export LESS='QRi'

# Force LibreOffice to use the generic Visual Components Library plugin.
export SAL_USE_VCLPLUGIN=gen

if [ -z "${PATH}" ]; then
    PATH='/usr/local/bin:/usr/bin:/bin'
fi

PATH="${PATH}:${HOME}/bin"

# Clean up PATH.  Do not modify PATH after this clean-up part.  Any
# modification to PATH must happen before this comment.
path_copy="${PATH}"
PATH=""
orig_IFS="${IFS+_${IFS}}"  # Note: ${foo+bar}, not ${foo:+bar}
IFS=':'
for p in ${path_copy}; do
    pathmunge "${p}" 'after'
done
if [ -z "${orig_IFS}" ]; then
    unset -v IFS
else
    IFS="${orig_IFS#_}"
fi
unset -v orig_IFS p path_copy

export PATH

unset -f pathmunge dir_r_x

# Source .bashrc in the end, and only if running bash.
if [ -n "${BASH_VERSION}" -a -r "${HOME}/.bashrc" ]; then
    . "${HOME}/.bashrc"
fi
