# ~/.profile
# Author: Raghu Rao <raghu.v.rao@gmail.com>

# This file must be fully sh-compatible as it is sourced by multiple sh-like
# shells!  No bashisms!

# Slightly modified version of pathmunge from Red Hat's /etc/profile.
pathmunge() {
  case ":${PATH}:" in
  *:"${1}":*) ;;
  *)
    if [ -z "${PATH}" ]; then  # Avoid leading/trailing colon
      PATH="${1}"
    else
      if [ "${2}" = "after" ]; then
        PATH="${PATH}:${1}"
      else
        PATH="${1}:${PATH}"
      fi
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

export VISUAL='emacsclient -c -t'
export ALTERNATE_EDITOR='emacs'
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

GOPATH="${HOME}/go"
if [ -d "${GOPATH}" ]; then
  export GOPATH
  if [ -d "${GOPATH}/bin" ]; then
    pathmunge "${GOPATH}/bin"
  fi
else
  unset GOPATH
fi

GOROOT="/opt/golang/root"
if [ -d "${GOROOT}" ]; then
  export GOROOT
  if [ -d "${GOROOT}/bin" ]; then
    pathmunge "${GOROOT}/bin"
  fi
else
  unset GOROOT
fi

# Do not modify PATH after this part (in other words: do this part towards the
# end of ~/.profile).  Remove any duplicates from PATH.  Order will be
# preserved.
orig_IFS="${IFS+_${IFS}}"  # Note: ${foo+bar}, not ${foo:+bar}
IFS=':'
path_copy="${PATH}"
PATH=""
for p in ${path_copy}; do
  pathmunge "${p}" "after"
done
if [ -z "${orig_IFS}" ]; then unset -v IFS; else IFS="${orig_IFS#_}"; fi
export PATH
unset -v orig_IFS p path_copy

unset -f pathmunge

# Source .bashrc in the end, and only if running bash.
if [ -n "${BASH_VERSION}" -a -r "${HOME}/.bashrc" ]; then
  . "${HOME}/.bashrc"
fi
