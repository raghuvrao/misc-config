# .bash_profile
# Raghu Rao

# Set variables here that will not change with each invocation of the shell,
# and will be available in all shells.
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
