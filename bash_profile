# .bash_profile

if [[ -d "${HOME}/bin" ]]; then
    export PATH="${PATH}:${HOME}/bin"
fi

if [[ -d "${HOME}/lib/python" ]]; then
    export PYTHONPATH="${PYTHONPATH}:${HOME}/lib/python"
fi

if [[ -f "${HOME}/.pythonrc.py" ]]; then
    export PYTHONSTARTUP="${HOME}/.pythonrc.py"
fi

if [[ -f "${HOME}/.bashrc" ]]; then
    . "${HOME}/.bashrc"
fi

export LESS='-R -i'
