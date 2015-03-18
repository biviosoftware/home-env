if [ $(expr "$BASH_SOURCE" : ~/src) = 0 -a -d ~/src/biviosoftware/home-env ]; then
    # Execute user's dot files only
    return
fi

# Undo some stuff
x="$(compgen -a)"
if [ ! -z "$x" ]; then
    unalias $x
fi
export LS_COLORS=
export USER_LS_COLORS=
export PROMPT_COMMAND=

dirs() {
    local f
    local -i i=0
    for f in `command dirs`; do
      	echo "    $i  $f"
      	i=$i+1
    done
}

b_ps1() {
    local x="["
    test "$1" && x="$x$1;"
    if [ "x$USER" != "x$LOGNAME" ]; then
        x="$x\u";
    fi
    if [ "x$DISPLAY" != 'x:0' ]; then
        x="$x@\h";
    fi
    PS1="$x \W]$bivio_ps1_suffix"
}

if bivio class info Bivio::BConf >& /dev/null; then
    bconf() {
        if [ -r /etc/$1.bconf ]; then
            export BCONF=/etc/$1.bconf
        else
            echo "Couldn't find BCONF=/etc/$1.bconf" 1>&2
            return 1
        fi
        b_ps1 $1
    }

    b() {
        bivio "$@"
    }

    bu() {
        bivio test unit "${@-.}"
    }

    ba() {
        bivio test acceptance "${@-.}"
    }

    if test $UID = 0; then
        function bi {
            local p=$1
            shift
            test -f /etc/$p.bconf && p=perl-app-$p
            bivio release install $p "$@"
        }
        bihs() {
            bivio release install_host_stream
        }
    fi
fi

g() {
    local x="$1"
    shift
    grep -Ir --exclude='.git' --exclude='*~' --exclude='.#*' --exclude='*/.#*' \
        "$x" "${@-.}" 2>/dev/null
}

gp() {
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

b_path_insert() {
    local dir="$1"
    local ignore_not_exist="$2"
    if [ \( "$ignore_not_exist" -o -d $dir \) -a $(expr ":$PATH:" : ".*:$dir:") = 0 ]; then
	export PATH="$dir:$PATH"
    fi
}

b_classpath_append() {
    local jar="$1"
    if [ $(expr ":$CLASSPATH:" : ".*:$jar:") = 0 ]; then
	export CLASSPATH=$CLASSPATH${CLASSPATH+:}$jar
    fi
}

unset BCONF
umask o-rwx
export LOGNAME=${LOGNAME:-$(logname)}

if [ ! -z "$PS1" ]; then
    if [ $UID = 0 ]; then
        bivio_ps1_suffix='# '
    else
        bivio_ps1_suffix='$ '
    fi
    if [ "x$TERM" != xdumb ]; then
        stty quit '^_'
    fi
    PS1="\W$bivio_ps1_suffix"
fi

export CVSUMASK=07
if test -z "$CVSROOT"; then
    if test -d /home/cvs/CVSROOT; then
	# We're on the CVS server
	export CVSROOT=/home/cvs
    elif test $(expr "$(hostname)" : '\(dfw4\|dfw1\|dfw3\|apa3\|apa11\|apa1\)\.'); then
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
    $(test $UID = 0 && echo /sbin /usr/sbin /usr/local/sbin) \
    ~/bin \
    ; do
    b_path_insert "$f"
done
unset f

if test $UID = 0 -o "$USER" = cvs; then
    export CVSREAD=true
fi

export JAVA_HOME=/usr/lib/jvm/java
#java -cp .:/usr/share/java/junit.jar org.junit.runner.JUnitCore
# Get most recent java and any jars in /usr/java
for f in $(ls /usr/java/*.jar 2> /dev/null); do
    b_classpath_append $f
done
unset f

if [ -d $HOME/src/java ]; then
    export JAVA_ROOT=$HOME/src/java
    b_classpath_append $JAVA_ROOT
fi

if [ -z "$PERLLIB" -a -d $HOME/src/perl ]; then
    export PERLLIB=$HOME/src/perl
fi
export FTP_PASSIVE=1

if test -f ~/.ssh/ssh_agent; then
    . ~/.ssh/ssh_agent > /dev/null
    if [ ! -z "$PS1" ]; then
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

if [ $(expr "$INSIDE_EMACS" : ".*comint") != 0 ]; then
    export PAGER=cat
    export EDITOR=$(type -path emacsclient)
    export NODE_NO_READLINE=1
    dirs() {
        echo "$DIRSTACK"
    }
    e() {
        emacsclient --no-wait "$@"
    }
else
    export PAGER=$(type -path less)
    export EDITOR=$(type -path emacs)
    e() {
        emacs "$@"
    }
fi

which() {
    type -path "$@"
}
clean() {
    find . -name '*~' -exec rm {} \;
}
