#!/bin/bash

# ~/.bash_profile
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

unset -v LESS
unset -v LESSOPEN
unset -v MANPATH

dir_r_x ()
{
    [[ -d "${1}" && -r "${1}" && -x "${1}" ]]
    return ${?}
}

path_append ()
{
    if dir_r_x "${1}"; then
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

# Remove duplicates and inaccessible directories from PATH.  Do not
# modify PATH after this clean-up part.  Any modification to PATH must
# happen before this comment.
path_copy="${PATH}"
PATH=""
orig_IFS="${IFS+_${IFS}}"  # Note: ${foo+bar}, not ${foo:+bar}
IFS=':'
for p in ${path_copy}; do
    path_append "${p}"
done
if [[ -z "${orig_IFS}" ]]; then
    unset -v IFS
else
    IFS="${orig_IFS#_}"
fi
unset -v orig_IFS p path_copy

export PATH

unset -f path_append dir_r_x

# Source ~/.bashrc in the end.
_bashrc="${HOME}/.bashrc"
if [[ -f "${_bashrc}" && -r "${_bashrc}" ]]; then
    source "${HOME}/.bashrc"
fi
unset -v _bashrc
