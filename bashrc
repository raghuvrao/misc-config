# .bashrc
# Raghu Rao

umask 077

# Do nothing if shell is non-interactive.
if [[ "${-}" != *i* ]]; then return; fi

unset LS_COLORS PROMPT_COMMAND
unalias -a

# Some general guidelines to tell if an environment variable belongs in this
# file:
#   - it is used only by the shell or shell builtins (e.g. PS1, HISTSIZE,
#     GLOBIGNORE etc.)
#   - it could change between shell sessions (e.g. different values for PS1
#     based on the value of TERM)
# Exporting such environment variables is typically unnecessary.
GLOBIGNORE='.:..'
HISTCONTROL='ignoreboth'
HISTSIZE=10000
HISTFILESIZE=20000
HISTTIMEFORMAT='%F %a %T %Z '
PROMPT_DIRTRIM=3
PS1='(\H)\w\$ '

shopt -u -o posix vi
shopt -s -o emacs pipefail
shopt -u dotglob
shopt -s checkhash checkwinsize cmdhist extglob histappend histreedit \
         histverify no_empty_cmd_completion sourcepath

ls_options='-a -b'
grep_options=''
cgrep_options=''

if [[ "${TERM}" =~ linux|xterm|screen && "${OSTYPE}" =~ linux ]]; then
    # Boldly assume all variants of the three terminal types
    # support color on linux.
    ls_options+=' --color=auto'
    grep_options+=' --color=auto'
    cgrep_options+=' --color=always'
fi

ls() { command ls ${ls_options} "${@}"; }

grep() { command grep ${grep_options} "${@}"; }

cgrep() { command grep ${cgrep_options} "${@}"; }

t()
{
    local unix_ts="${1}"
    local include_now_info='yes'
    if [[ -z "${unix_ts}" ]]; then
        unix_ts="$(date +%s)"
        include_now_info='no'
    elif [[ ! "${unix_ts}" =~ ^(\+|\-)?[0-9]+(\.[0-9]+)?$ ]]; then
        echo "Usage: ${FUNCNAME[0]} [UNIX timestamp]" >&2
        return 1
    fi
    python <<END
import sys
from datetime import datetime as dt
try:
    from pytz import timezone, utc
except ImportError, i:
    print "Cannot import pytz: {0}".format(i)
    sys.exit(1)
tf = '%F %a %T %Z(%z)'
tzs = [timezone('Etc/UTC'),
       timezone('US/Pacific'),
       timezone('US/Eastern'),
       timezone('Asia/Kolkata'),
       timezone('Asia/Shanghai')]
unix_ts = int(float('${unix_ts}'))
my_dt = dt.utcfromtimestamp(unix_ts).replace(tzinfo=utc)
now = None
if '${include_now_info}' == 'yes':
    import time
    now = int(time.time())
    now_dt = dt.utcfromtimestamp(now).replace(tzinfo=utc)
print "{0} {1}UNIX".format(
    unix_ts,
    "(now: {0}; diff: {1}) ".format(now, now - unix_ts) if now else "")
for tz in tzs:
    print "{0} {1}{2}".format(
        my_dt.astimezone(tz).strftime(tf),
        "(now: {0}) ".format(now_dt.astimezone(tz).strftime(tf)) if now else "",
        tz)
END

}
