export BIVIO_SRC_HOME_ENV=~/src/biviosoftware/home-env
bivio_not_src_home_env() {
    local d=$BIVIO_SRC_HOME_ENV
    if [[ ! ( ${BASH_SOURCE[0]} =~ $d ) && -d $d ]]; then
        return 0
    fi
    return 1

}

if bivio_not_src_home_env; then
    # Execute user's dot files only
    return
fi

# Undo some stuff
for f in $(compgen -a); do
    unalias "$f"
done
unset f

# Undo bivio functions from /etc/bashrc.d
for f in bconf b bu ba bi bihs ctd g gp $(compgen -A function | egrep '^(b_|bivio_)'); do
    if [[ $f != bivio_not_src_home_env ]]; then
        unset -f "$f"
    fi
done
unset f

umask g-w,o-rwx
export LS_COLORS=
export USER_LS_COLORS=
export PROMPT_COMMAND=
export VAGRANT_NO_COLOR=true
export USER=${USER:-$(id -u -n)}
export LOGNAME=${LOGNAME:-$(logname 2>/dev/null || echo $USER)}
unset BCONF

# python pip installs in /tmp, which doesn't work if the package is large
# and /tmp is on tmpfs.
if [[ ! $TMPDIR && $(df /tmp 2>&1 | tail -1) =~ tmpfs ]]; then
    export TMPDIR=${TMPDIR-/var/tmp}
fi

dirs() {
    local f
    local -i i=0
    for f in $(command dirs); do
      	echo "    $i  $f"
      	i=$i+1
    done
}

bivio_ps1() {
    if [[ -z $PS1 ]]; then
        return
    fi
    local x='['
    if [[ -n $1 ]]; then
        x="$x$1;"
    fi
    if [[ $USER != $LOGNAME ]]; then
        x="$x\u";
    fi
    if [[ ! -f /.dockerinit && $DISPLAY != :0 ]]; then
        x="$x@\h"
    fi
    PS1="$x \W]$bivio_ps1_suffix"
}

if [[ -n $PS1 ]]; then
    bivio_prompt_command() {
        case $TERM in
        xterm*)
            printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"
            ;;
        screen)
            printf "\033]0;%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"
            ;;
        *)
            ;;
        esac
    }
    if [[ $EUID == 0 ]]; then
        bivio_ps1_suffix='# '
    else
        bivio_ps1_suffix='$ '
    fi
    if [[ $TERM != dumb ]]; then
        stty quit '^_'
    fi
    PS1="\W$bivio_ps1_suffix"
    PROMPT_COMMAND=bivio_prompt_command
    bivio_prompt_command
fi

if bivio class info Bivio::BConf >& /dev/null; then
    b() {
        bivio "$@"
    }

    if [[ ~/src/biviosofware/perl-Bivio ]]; then
        bu() {
            bivio test unit "${@-.}"
        }

        ba() {
            bivio test acceptance "${@-.}"
        }
    fi

    if [[ $EUID == 0 ]]; then
        bconf() {
            if [[ -r /etc/$1.bconf ]]; then
                export BCONF="/etc/$1.bconf"
            else
                echo "Couldn't find BCONF=/etc/$1.bconf" 1>&2
                return 1
            fi
            bivio_ps1 $1
        }

        bi() {
            local p="$1"
            shift
            if [[ -f /etc/$p.bconf ]]; then
                p="perl-app-$p"
            fi
            bivio release install "$p" "$@"
        }

        bihs() {
            bivio release install_host_stream
        }
    fi
fi

g() {
    local x="$1"
    shift
    grep -iIr --exclude-dir='.git' --exclude='*~' --exclude='.#*' --exclude='*/.#*' \
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

bivio_path_dedup() {
    export PATH=$(perl -e 'print(join(q{:}, grep(!$x{$_}++, split(/:/, $ENV{PATH}))))')
}

bivio_path_insert() {
    local dir="$1"
    local ignore_not_exist="$2"
    if [[ ( $ignore_not_exist || -d $dir ) && ! ( :$PATH: =~ :$dir: ) ]]; then
	export PATH="$dir:$PATH"
    fi
}

bivio_path_remove() {
    export PATH=$(perl -e 'print(join(q{:}, grep($_ ne $ARGV[0], split(/:/, $ENV{PATH}))))' "$1")
}

bivio_classpath_append() {
    local jar="$1"
    if [[ ! ( :$CLASSPATH: =~ :$jar: ) ]]; then
	export CLASSPATH=$CLASSPATH${CLASSPATH+:}$jar
    fi
}

export CVSUMASK=07
if [[ -z $CVSROOT ]]; then
    if [[ -d /home/cvs/CVSROOT ]]; then
	# We're on the CVS server
	export CVSROOT=/home/cvs
    elif [[ $HOSTNAME =~ (dfw1|apa3|apa11|apa1)(\.|$) ]]; then
	# direct connect
	export CVSROOT=":pserver:$LOGNAME@locker.bivio.biz:/home/cvs"
    else
	# We're remote and do SSH tunneling
	export CVSROOT=":pserver:$LOGNAME@localhost:/home/cvs"
    fi
fi

for f in \
    /usr/lib64/openmpi/bin \
    /usr/local/cuda/bin \
    $(ls -td /usr/java/{jdk*,jre*} /opt/IBMJava* 2>/dev/null) \
    /usr/local/bin \
    /opt/local/bin \
    $( [[ $EUID == 0 ]] && echo /sbin /usr/sbin /usr/local/sbin /opt/local/sbin) \
    ~/bin \
    ; do
    bivio_path_insert "$f"
done
unset f

if [[ -d /usr/lib64/openmpi/lib ]]; then
    export LD_LIBRARY_PATH=/usr/lib64/openmpi/lib${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
fi

if [[ $EUID == 0 || $USER == cvs ]]; then
    export CVSREAD=true
fi

export JAVA_HOME=/usr/lib/jvm/java
#java -cp .:/usr/share/java/junit.jar org.junit.runner.JUnitCore
# Get most recent java and any jars in /usr/java
for f in $(ls /usr/java/*.jar 2> /dev/null); do
    bivio_classpath_append $f
done
unset f

if [[ -d $HOME/src/java ]]; then
    export JAVA_ROOT=$HOME/src/java
    bivio_classpath_append $JAVA_ROOT
fi

if [[ -z $PERLLIB && -d ~/src/perl ]]; then
    export PERLLIB=$HOME/src/perl
fi
export FTP_PASSIVE=1

if [[ -f ~/.ssh/ssh_agent ]]; then
    . ~/.ssh/ssh_agent > /dev/null
    if [[ -n "$PS1" ]]; then
        if ! ps ${SSH_AGENT_PID-0} 2>&1 | grep -s -q ssh-agent; then
	    # Start a daemon and add
	    ssh-agent > ~/.ssh/ssh_agent
	    . ~/.ssh/ssh_agent
	    ssh-add
            x=~/.vagrant.d/insecure_private_key
            if [[ -f $x  ]]; then
                ssh-add $x
            fi
            unset x
	fi
    fi
fi

if [[ $INSIDE_EMACS =~ comint ]]; then
    # It's probably dumb, but force to be sure
    export TERM=dumb

    dirs() {
        echo "$DIRSTACK"
    }

    e() {
        emacsclient --no-wait "$@"
    }
else
    export PAGER=$(type -p less)
    export EDITOR=$(type -p emacs)

    e() {
        emacs "$@"
    }
fi
if [[ $TERM == dumb ]]; then
    export EDITOR=$(type -p emacsclient)
    export NODE_NO_READLINE=1
    export PAGER=cat
    export SYSTEMD_COLOR=0
fi

which() {
    type "$@"
}

clean() {
    find . -name '*~' -exec rm {} \;
}
