# .bash_profile
# Raghu Rao

_dirs_=( "${HOME}/bin" )
for _dir_ in ${_dirs_[@]}; do
    if [[ ( -d "${_dir_}" ) && ( ! "${PATH}" =~ ${_dir_} ) ]]; then
        PATH="${PATH}:${_dir_}"
    fi
done
unset _dir_ _dirs_

export LESS='-R -i'

if [[ -d "${HOME}/lib/python" ]]; then
    export PYTHONPATH="${PYTHONPATH}:${HOME}/lib/python"
fi

if [[ -f "${HOME}/.pythonrc.py" ]]; then
    export PYTHONSTARTUP="${HOME}/.pythonrc.py"
fi

# Probably a good idea to source stuff _after_ doing everything else.
if [[ -f "${HOME}/.bashrc" ]]; then
    . "${HOME}/.bashrc"
fi
