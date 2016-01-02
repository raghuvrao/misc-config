# .bash_profile
# Raghu Rao

# from /etc/profile (Red Hat)
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

# If an environment variable does not change between shell invocations and is
# required in child shells or forked programs it belongs in .profile or
# .bash_profile.  Such variables must be exported so they can be made available
# in child shells and other programs the shell forks and execs.

export VISUAL='vim'
export EDITOR="${VISUAL}"
export FCEDIT="${VISUAL}"
export SVN_EDITOR="${VISUAL}"
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

export PATH
unset -f pathmunge

# Source .bashrc in the end, and only if running bash.
if [ -n "${BASH_VERSION}" -a -r "${HOME}/.bashrc" ]; then
  . "${HOME}/.bashrc"
fi
