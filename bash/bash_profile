#!/bin/bash

# ~/.bash_profile
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

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

export NO_COLOR=1

[[ -z "${PATH}" ]] && PATH='/usr/local/bin:/usr/bin:/bin'

PATH="${HOME}/.local/bin:${PATH}"

source "${HOME}/.bash_profile_local.bash"

# ---- KEEP YOUR MODIFICATIONS TO PATH BEFORE THIS COMMENT! ----
#
# Remove empty entries, ., .., duplicates, and inaccessible directories from
# PATH while preserving order.
#
# Note: ${foo+bar} below, not ${foo:+bar}
original_IFS="${IFS+_${IFS}}"
IFS=':'
declare -a paths
declare -A paths_seen
for p in ${PATH}; do
    if [[ -n "${p}" && "${p}" != '.' && "${p}" != '..' && -z "${paths_seen["${p}"]+_}" && -d "${p}" && -r "${p}" && -x "${p}" ]]; then
        paths_seen["${p}"]=1
        paths+=("${p}")
    fi
done
# The '*' below makes Bash use the first character of IFS (instead of space) as
# the delimiter when constructing a string from the 'paths' array.
PATH="${paths[*]}"
# Restore IFS.
if [[ -z "${original_IFS}" ]]; then
    unset -v IFS
else
    IFS="${original_IFS#_}"
fi
unset -v original_IFS paths paths_seen

export PATH

# Source ~/.bashrc in the end.
source "${HOME}/.bashrc"
