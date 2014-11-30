# -*-sh-*-
if [ $(expr match "$BASH_SOURCE" ~/src) == 0 -a -d ~/src/biviosoftware/home-env ]; then
    return
fi
bivio_base_sh=true
unset BCONF
umask o-rwx
export LOGNAME=${LOGNAME:-$(logname)}

if [ ! -z "$PS1" -a "x$TERM" != xdumb ]; then
    stty quit '^_'
fi

export CVSUMASK=07
if test -z "$CVSROOT"; then
    if test -d /home/cvs/CVSROOT; then
	# We're on the CVS server
	export CVSROOT=/home/cvs
    elif test $(expr match "$(hostname)" '\(dfw4\|dfw1\|dfw3\|apa3\|apa11\|apa1\)\.'); then
	# direct connect
	export CVSROOT=":pserver:$LOGNAME@locker.bivio.biz:/home/cvs"
    else
	# We're remote and do SSH tunneling
	export CVSROOT=":pserver:$LOGNAME@localhost:/home/cvs"
    fi
fi

for f in \
    $(ls -td /usr/java/{jdk*,jre*} /opt/IBMJava* 2>/dev/null) \
    /opt/local/bin \
    /usr/local/bin \
    /usr/local/cuda/bin \
    /usr/local/anaconda/bin \
    $(test $UID = 0 && echo /sbin /usr/sbin /usr/local/sbin) \
    ~/bin \
    ; do
    if [ -d $f -a $(expr match ":$PATH:" ".*:$f:") = 0 ]; then
	export PATH="$f:$PATH"
    fi
done
unset f

if test $UID = 0 -o "$USER" = cvs; then
    export CVSREAD=true
fi

if test $UID = 0; then
    function bi {
        local p=$1
	shift
        test -f /etc/$p.bconf && p=perl-app-$p
        bivio release install $p "$@"
    }
    function bihs {
        bivio release install_host_stream
    }
fi

# Get most recent java and any jars in /usr/java
for f in $(ls /usr/java/*.jar 2> /dev/null); do
    export CLASSPATH=$CLASSPATH${CLASSPATH+:}$f
done
unset f

if [ -d $HOME/src/java ]; then
    export JAVA_ROOT=$HOME/src/java
    export CLASSPATH="$CLASSPATH:$JAVA_ROOT"
fi

if [ -z "$PERLLIB" -a -d $HOME/src/perl ]; then
    export PERLLIB=$HOME/src/perl
fi
export FTP_PASSIVE=1

if test -f ~/.ssh/ssh_agent; then
    . ~/.ssh/ssh_agent > /dev/null
    if [ "$PS1" ]; then
        if ps ${SSH_AGENT_PID-0} 2>&1 | grep ssh-agent > /dev/null 2>&1; then
	    : We have a daemon
	else
	    # Start a daemon and add
	    ssh-agent > ~/.ssh/ssh_agent
	    . ~/.ssh/ssh_agent
	    ssh-add
            (x=~/.vagrant.d/insecure_private_key && test -f $x && ssh-add $x)
	fi
    fi
fi

# must be "function", because "dirs" may be an alias. "dirs () { " will throw
# an exception in that case
function dirs {
    local f
    local -i i=0
    for f in `command dirs`; do
      	echo "    $i  $f"
      	i=$i+1
    done
}

function b_ps1 {
    local x="["
    test "$1" && x="$x$1;"
    if [ "x$USER" != "x$LOGNAME" ]; then
    	x="$x\u";
    fi
    if [ "x$DISPLAY" != 'x:0' ]; then
    	x="$x@\h";
    fi
    PS1="$x \W]\\$ "
}

function bconf {
    if [ -r /etc/$1.bconf ]; then
        export BCONF=/etc/$1.bconf
    else
        echo "Couldn't find BCONF=/etc/$1.bconf" 1>&2
        return 1
    fi
    b_ps1 $1
}

function bu {
    bivio test unit "${@-.}"
}

function ba {
    bivio test acceptance "${@-.}"
}

function g {
    local x="$1"
    shift
    grep -Ir --exclude='*~' --exclude='.#*' --exclude='*/.#*' \
        "$x" "${@-.}" 2>/dev/null
}

function gp {
    local x="$1"
    shift
    g --include '*.btest' \
	--include '*.bunit' \
	--include '*.t' \
	--include '*.pm' \
	--include '*.pl' \
	--include '*.PL' \
	--include '*.py' \
        "$x" "${@-.}" |
	egrep -v '/old/|/files/artisans/plain/f/bOP|Util/t/Dev.tmp|/pkgs/(build|tmp)|Bivio/bOP.pm'
}

if [ "x$TERM" = xdumb ]; then
    # Emacs needs a different version of dirs
    alias dirs='echo $DIRSTACK'
fi

alias b=bivio
alias e='emacsclient --no-wait'
alias which="type -path"
alias "rm~=find . -name '*~' -exec rm {} ';'"
