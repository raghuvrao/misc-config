#!/bin/zsh

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

unset -v LESS LESSOPEN

typeset -U PATH path FPATH path MANPATH manpath

if (( ${#path} == 0 )); then
	path=(/usr/local/bin /usr/bin /bin)
fi

path+=(/usr/local/sbin /usr/sbin /sbin)

path=(${HOME}/.local/bin ${HOME}/bin ${path})
