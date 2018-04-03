# ~/.profile
# Author: Raghu Rao <raghu.v.rao@gmail.com>
#
# Keep this file so POSIX-compliant as possible because it is sourced by
# multiple sh-like shells.  No bashisms!

# Slightly modified version of pathmunge from Red Hat's /etc/profile.
pathmunge() {
	if test ! -d "${1}"; then
		return
	fi
	case ":${PATH}:" in
		*:"${1}":*)
			;;
		*)
			# Avoid leading/trailing colon.
			if [ -z "${PATH}" ]; then
				PATH="${1}"
				return
			fi
			if [ "${2}" = "after" ]; then
				PATH="${PATH}:${1}"
			else
				PATH="${1}:${PATH}"
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

export VISUAL='vim'
export EDITOR="${VISUAL}"
export FCEDIT="${VISUAL}"
export SVN_EDITOR="${VISUAL}"

export PAGER='less'
export LESS='QRi'
export GIT_PAGER='less -+F -X'

# Force LibreOffice to use the generic Visual Components Library plugin.
# I find the others (kde4, gtk, and gtk3) ugly.
export SAL_USE_VCLPLUGIN=gen

export mcfg="${HOME}/src/git/misc-config"
export mscr="${HOME}/src/git/misc-scripts"

if [ -z "${PATH}" ]; then
	PATH='/usr/local/bin:/usr/bin:/bin'
fi

PATH="${PATH}:/usr/local/sbin:/usr/sbin:/sbin"

p='/opt/golang/root'
if [ -d "${p}" ]; then
	export GOROOT="${p}"
	PATH="${GOROOT}/bin:${PATH}"
fi

p="${HOME}/go"
if [ -d "${p}" ]; then
	export GOPATH="${p}"
	PATH="${PATH}:${GOPATH}/bin"
fi

unset -v p

PATH="${PATH}:${HOME}/bin"

# Do not modify PATH after this part (in other words: do this part towards the
# end of ~/.profile).  Remove any duplicates from PATH.  Order will be
# preserved.  Non-existent directories will be removed.
orig_IFS="${IFS+_${IFS}}"  # Note: ${foo+bar}, not ${foo:+bar}
IFS=':'
path_copy="${PATH}"
PATH=""
for p in ${path_copy}; do
	pathmunge "${p}" 'after'
done
if [ -z "${orig_IFS}" ]; then unset -v IFS; else IFS="${orig_IFS#_}"; fi
unset -v orig_IFS p path_copy

unset -f pathmunge

# Finally, export PATH after cleaning it up.
export PATH

# Source .bashrc in the end, and only if running bash.
if [ -n "${BASH_VERSION}" -a -r "${HOME}/.bashrc" ]; then
	. "${HOME}/.bashrc"
fi
