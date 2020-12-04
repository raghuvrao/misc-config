#!/bin/zsh

# Many programs that invoke 'less' themselves for paginating their
# output set LESS to a value they prefer if LESS is not already set.
# I cannot seem to find a combination of options to put into LESS that
# works for every use-case.  So, let the programs that run 'less' decide
# what they want in LESS themselves.
unset -v LESS

# When I run the 'less' program, I want it not to do much more than
# paginating the contents of a file.  I do not want 'less' invoking
# external programs to process the input files, and paginating the
# output of those external programs.  So, unset LESSOPEN to prevent
# 'less' from invoking other programs to process/display files.
unset -v LESSOPEN

for v in "${HOME}/.local/bin/vim" '/usr/local/bin/vim' '/usr/bin/vim'; do
    if [[ -f "${v}" && -x "${v}" ]]; then
        export VISUAL="${v}"
        break
    fi
done

if [[ -n "${VISUAL}" ]]; then
    export EDITOR="${VISUAL}"
fi

# Force LibreOffice to use the generic Visual Components Library plugin.
export SAL_USE_VCLPLUGIN=gen

export mcfg="${HOME}/src/git/misc-config"
export mscr="${HOME}/src/git/misc-scripts"

typeset -U PATH path FPATH path

if (( ${#path} == 0 )); then
    path=(/usr/local/bin /usr/bin /bin)
fi

path+=(/usr/local/sbin /usr/sbin /sbin)

path=(${HOME}/.local/bin ${path})
