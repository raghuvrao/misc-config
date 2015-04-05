# .bash_profile
# Raghu Rao

# The order of items in _dirs_ below is important for the regex-matching
# following it to work properly.  Think about what would happen if /usr/sbin
# were before /sbin in _dirs_.  Stupid, but it works: we are not dealing with
# too many items in _dirs_, so manually resolving the order is feasible.
_dirs_=( "${HOME}/bin" "/sbin" "/usr/sbin" "/usr/local/sbin" )
for _dir_ in ${_dirs_[@]}; do
    if [[ ( -d "${_dir_}" ) && ( ! "${PATH}" =~ ${_dir_} ) ]]; then
        PATH="${PATH}:${_dir_}"
    fi
done
unset _dir_ _dirs_

# Se variables here that will not change with each invocation of the shell, and
# will be available in all shells.
export LESS='-R -i'
export VISUAL='vim'
export EDITOR="${VISUAL}"
export FCEDIT="${VISUAL}"
export HISTCONTROL='ignoreboth'
export HISTSIZE=10000
export HISTFILESIZE=20000
export HISTTIMEFORMAT='%F %T '
export GLOBIGNORE='.:..'

# Should I ever need to install python libs in my homedir.
if [[ -d "${HOME}/lib/python" ]]; then
    export PYTHONPATH="${PYTHONPATH}:${HOME}/lib/python"
fi

# Here to remind me that the python interpreter supports a startup /
# run-control / initialization file.
if [[ -f "${HOME}/.pythonrc.py" ]]; then
    export PYTHONSTARTUP="${HOME}/.pythonrc.py"
fi

# Source the shell's run control file _after_ doing everything else.
if [[ -f "${HOME}/.bashrc" ]]; then
    . "${HOME}/.bashrc"
fi
