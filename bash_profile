#!/bin/bash

# ~/.bash_profile
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

unset -v LESS
unset -v LESSOPEN
unset -v MANPATH

dir_r_x ()
{
    [[ -d "${1}" && -r "${1}" && -x "${1}" ]]
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
#   1.  Its value remains the same in all shell invocations.
#
#   2.  It is expected to be available to child processes (e.g. programs
#       other than the shell use it).
#
#   3.  It does not concern interactive shell sessions in particular
#       (e.g. PS1 is interactive-only, so it does not belong in this
#       file).
#
# To make a variable available to child processes, it must be exported.

export VISUAL='vim'

# Force LibreOffice to use the generic Visual Components Library plugin.
export SAL_USE_VCLPLUGIN=gen

export mcfg="${HOME}/src/git/misc-config"
export mscr="${HOME}/src/git/misc-scripts"

if [[ -z "${PATH}" ]]; then
    PATH='/usr/local/bin:/usr/bin:/bin'
fi

PATH+=":/usr/local/sbin:/usr/sbin:/sbin"

PATH="${HOME}/.local/bin:/opt/local/bin:${PATH}"

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
x="${HOME}/.bashrc"
if [[ -f "${x}" && -r "${x}" ]]; then
    source "${x}"
fi
unset -v x
