#!/bin/sh

# .bash_profile
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

# dir_r_x returns true if its first argument is a readable directory into which
# we can descend; false otherwise.
dir_r_x()
{
    test -d "${1}" -a -r "${1}" -a -x "${1}"
    return ${?}
}

# path_append() appends its first argument to PATH if the first argument is
# a readable directory into which I can descend; otherwise, path_append() does
# nothing to PATH.  This function is a modified version of pathmunge() from
# Fedora's /etc/profile.
path_append()
{
    # Include only directories that we can both read and into which we can
    # descend.
    if dir_r_x "${1}"; then
        case ":${PATH}:" in
            (::)
                # PATH is empty; avoid leading/trailing colon.
                PATH="${1}"
                ;;
            (*:"${1}":*)
                # PATH already has ${1} in it; do nothing.
                ;;
            (*)
                PATH="${PATH}:${1}"
                ;;
        esac
    fi
}

# Some general guidelines to tell if an environment variable belongs in this
# file:
#
# 1.  The variable's value remains the same in all shell invocations.
#
# 2.  Programs other than the shell use the variable.  To make a variable
#     available to child processes, it must be exported.
#
# 3.  The variable does not concern interactive shell processes in particular.
#     For example, the variable PS1 is relevant only for interactive shells,
#     and so it does not belong in this file.

export VISUAL='vim'
export EDITOR="${VISUAL}"

# Force LibreOffice to use the generic Visual Components Library plugin.
export SAL_USE_VCLPLUGIN=gen

unset -v LESS LESSOPEN

# Slackware sets MANPATH (even though /etc/man_db.conf is configured
# satisfactorily, so far as I go).  Slackware's default MANPATH causes
# problems sometimes.  E.g. when two versions of a program are installed in
# two different locations, sometimes man pulls up the version of the man page
# that does not match the version of the command.  Unsetting MANPATH seems to
# solve this problem.
unset -v MANPATH

if [ -z "${PATH}" ]; then
    PATH='/usr/local/bin:/usr/bin:/bin'
fi

PATH="${HOME}/.local/bin:${HOME}/bin:${PATH}"

# Clean up PATH.  Do not modify PATH after this clean-up part.  Any
# modification to PATH must happen before this comment.
path_copy="${PATH}"
PATH=""
orig_IFS="${IFS+_${IFS}}"  # Note: ${foo+bar}, not ${foo:+bar}
IFS=':'
for p in ${path_copy}; do
    path_append "${p}"
done
if [ -z "${orig_IFS}" ]; then
    unset -v IFS
else
    IFS="${orig_IFS#_}"
fi
unset -v orig_IFS p path_copy

export PATH

unset -f path_append dir_r_x

# Source .bashrc in the end, and only if running bash.
if [ -n "${BASH_VERSION}" -a -r "${HOME}/.bashrc" ]; then
    . "${HOME}/.bashrc"
fi
