#!/bin/sh

# ~/.profile
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>
#
# Keep this file so POSIX-compliant as possible because it is sourced by
# multiple sh-like shells.  No bashisms!

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
#   1.  Its value remains the same in all shell invocations.
#
#   2.  It is expected to be available to child processes (e.g. programs other
#       than the shell use it).
#
#   3.  It does not concern interactive shell sessions in particular (e.g. PS1
#       is interactive-only, so it does not belong in this file).
#
#   4.  It is not bash-specific.
#
# To make a variable available to child processes, it must be exported.

my_visual_editor='/usr/local/bin/vim'
if [ -f "${my_visual_editor}" -a -x "${my_visual_editor}" ]; then
    export VISUAL="${my_visual_editor}"
else
    export VISUAL='/usr/bin/vim'
fi
unset -v my_visual_editor
export EDITOR="${VISUAL}"

export PAGER='/usr/bin/less'

export LESS=' -R -m'

# Force LibreOffice to use the generic Visual Components Library plugin.
# I find the others (kde4, gtk, and gtk3) ugly.
export SAL_USE_VCLPLUGIN=gen

export mcfg="${HOME}/src/git/misc-config"
export mscr="${HOME}/src/git/misc-scripts"

if [ -z "${PATH}" ]; then
    PATH='/usr/local/bin:/usr/bin:/bin'
fi

PATH="${PATH}:/usr/local/sbin:/usr/sbin:/sbin"

p='/opt/golang/root'
if dir_r_x "${p}" && dir_r_x "${p}/bin"; then
    export GOROOT="${p}"
    PATH="${GOROOT}/bin:${PATH}"
fi

p="${HOME}/go"
if dir_r_x "${p}" && dir_r_x "${p}/bin"; then
    export GOPATH="${p}"
    PATH="${GOPATH}/bin:${PATH}"
fi

unset -v p

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
