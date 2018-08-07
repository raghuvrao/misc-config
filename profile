#!/bin/sh

# ~/.profile
# Author: Raghu Rao <raghu.v.rao@gmail.com>
#
# Keep this file so POSIX-compliant as possible because it is sourced by
# multiple sh-like shells.  No bashisms!

# Slightly modified version of pathmunge from Red Hat's /etc/profile.
pathmunge() {
    # Include only directories that we can both read and into which we can
    # descend.
    if [ -d "${1}" -a -r "${1}" -a -x "${1}" ]; then
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
# I find the others (kde4, gtk, and gtk3) ugly.
export SAL_USE_VCLPLUGIN=gen

export mcfg="${HOME}/src/git/misc-config"
export mscr="${HOME}/src/git/misc-scripts"

if [ -z "${PATH}" ]; then
    PATH='/usr/local/bin:/usr/bin:/bin'
fi

PATH="${PATH}:/usr/local/sbin:/usr/sbin:/sbin"

p='/opt/golang/root'
if [ -d "${p}" ]; then
    export GOROOT="${p}"
    PATH="${GOROOT}/bin:${PATH}"
fi

p="${HOME}/go"
if [ -d "${p}" ]; then
    export GOPATH="${p}"
    PATH="${PATH}:${GOPATH}/bin"
fi

unset -v p

PATH="${PATH}:${HOME}/bin"

# Clean up PATH.  Do not modify PATH after this part (in other words: do this
# part towards the end of ~/.profile).
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

unset -f pathmunge

export PATH

# Source .bashrc in the end, and only if running bash.
if [ -n "${BASH_VERSION}" -a -r "${HOME}/.bashrc" ]; then
    . "${HOME}/.bashrc"
fi
