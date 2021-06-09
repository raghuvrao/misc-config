#!/bin/bash

# ~/.bash_profile
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

unset -v LESS
unset -v LESSOPEN
unset -v MANPATH

_s ()
{
    if [[ -f "${1}" && -r "${1}" ]]; then
        source "${1}"
    fi
}

path_append ()
{
    if [[ -n "${1}" && -d "${1}" && -r "${1}" && -x "${1}" ]]; then
        case ":${PATH}:" in
            (::)
                PATH="${1}"
                ;;
            (*:"${1}":*)
                ;;
            (*)
                PATH="${PATH}:${1}"
                ;;
        esac
    fi
}

# Some general guidelines to tell if an environment variable belongs in
# this file:
#
# 1.  The variable's value remains the same in all shell invocations.
#
# 2.  Programs other than the shell use the variable.  To make
#     a variable available to child processes, it must be exported.
#
# 3.  The variable does not concern interactive shell processes in
#     particular.  For example, the variable PS1 is relevant only for
#     interactive shells, and so it does not belong in this file.

export VISUAL='vim'
export EDITOR="${VISUAL}"

# Force LibreOffice to use the generic Visual Components Library plugin.
export SAL_USE_VCLPLUGIN=gen

if [[ -z "${PATH}" ]]; then
    PATH='/usr/local/bin:/usr/bin:/bin'
fi

PATH="${HOME}/.local/bin:${PATH}"

_s "${HOME}/.bash_profile_local.bash"

# Remove duplicates and inaccessible directories from PATH.  Do not
# modify PATH after this clean-up part.  Any modification to PATH must
# happen before this comment.
path_copy="${PATH}"
PATH=""
# Note: ${foo+bar} below, not ${foo:+bar}
original_IFS="${IFS+_${IFS}}"
IFS=':'
for p in ${path_copy}; do
    path_append "${p}"
done
if [[ -z "${original_IFS}" ]]; then
    unset -v IFS
else
    IFS="${original_IFS#_}"
fi
export PATH
unset -v original_IFS path_copy

# Source ~/.bashrc in the end.
_s "${HOME}/.bashrc"

unset -f path_append
unset -f _s
