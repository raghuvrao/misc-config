#!/bin/zsh

# Many programs that invoke 'less' themselves for paginating their output set
# LESS to a value they prefer if LESS is not already set.  I cannot seem to
# find a combination of options to put into LESS that works for every
# use-case.  So, let the programs that run 'less' decide what they want in
# LESS themselves.
unset -v LESS

# When I run the 'less' program, I want it not to do much more than paginating
# the contents of a file.  I do not want 'less' invoking external programs to
# process the input files, and paginating the output of those external
# programs.  So, unset LESSOPEN to prevent 'less' from invoking other programs
# to process/display files.
unset -v LESSOPEN

my_visual_editor='/usr/local/bin/vim'
if [[ -f "${my_visual_editor}" && -x "${my_visual_editor}" ]]; then
	export VISUAL="${my_visual_editor}"
else
	export VISUAL='/usr/bin/vim'
fi
unset -v my_visual_editor
export EDITOR="${VISUAL}"

export PAGER='/usr/bin/less'

# Force LibreOffice to use the generic Visual Components Library plugin.
# I find the others (kde4, gtk, and gtk3) ugly.
export SAL_USE_VCLPLUGIN=gen

export mcfg="${HOME}/src/git/misc-config"
export mscr="${HOME}/src/git/misc-scripts"

typeset -U PATH path FPATH path

if (( ${#path} == 0 )); then
	path=(/usr/local/bin /usr/bin /bin)
fi

path+=(/usr/local/sbin /usr/sbin /sbin)

path=(${HOME}/.local/bin ${HOME}/bin ${path})
