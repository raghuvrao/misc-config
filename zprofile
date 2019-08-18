# dir_r_x returns true if its first argument is a readable directory into
# which we can descend; false otherwise.
dir_r_x () {
	[[ -d "${1}" && -r "${1}" && -x "${1}" ]]
	return ${?}
}

my_visual_editor='/usr/local/bin/vim'
if [[ -f "${my_visual_editor}" && -x "${my_visual_editor}" ]]; then
	export VISUAL="${my_visual_editor}"
else
	export VISUAL='/usr/bin/vim'
fi
unset -v my_visual_editor
export EDITOR="${VISUAL}"

export PAGER='/usr/bin/less'

export LESS=' -R -m'

# Force LibreOffice to use the generic Visual Components Library plugin.
# I find the others (kde4, gtk, and gtk3) ugly.
export SAL_USE_VCLPLUGIN=gen

export mcfg="${HOME}/src/git/misc-config"
export mscr="${HOME}/src/git/misc-scripts"

typeset -U PATH path

if (( ${#path} == 0 )); then
	path=(/usr/local/bin /usr/bin /bin)
fi

path+=(/usr/local/sbin /usr/sbin /sbin)

p='/opt/go/root'
if dir_r_x "${p}" && dir_r_x "${p}/bin"; then
	export GOROOT="${p}"
	path=(${GOROOT}/bin ${path})
fi

p="${HOME}/go"
if dir_r_x "${p}" && dir_r_x "${p}/bin"; then
	export GOPATH="${p}"
	path=(${GOPATH}/bin ${path})
fi

unset -v p

path=(${HOME}/.local/bin ${HOME}/bin ${path})

unhash -f dir_r_x
