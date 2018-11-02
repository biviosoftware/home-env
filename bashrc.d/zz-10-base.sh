export BIVIO_SRC_HOME_ENV="$HOME"/src/biviosoftware/home-env
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
if [[ ! ${TMPDIR:-} && $(df /tmp 2>&1 | tail -1) =~ tmpfs ]]; then
    export TMPDIR=${TMPDIR-/var/tmp}
fi

if [[ -z $TZ && -e /etc/localtime ]]; then
    # Tested on CentOS 7, and it does have the localtime stat problem
    # https://blog.packagecloud.io/eng/2017/02/21/set-environment-variable-save-thousands-of-system-calls/
    export TZ=:/etc/localtime
fi

dirs() {
    local f
    local -i i=0
    for f in $(command dirs); do
      	echo "    $i  $f"
      	i=$i+1
    done
}

bivio_in_docker() {
    grep -s -q cpuset:/docker /proc/self/cgroup >& /dev/null
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
    if ! bivio_in_docker && [[ ${DISPLAY:-} != :0 ]]; then
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

bivio_path_dedup() {
    export PATH=$(perl -e 'print(join(q{:}, grep(!$x{$_}++, split(/:/, $ENV{PATH}))))')
}

bivio_path_insert() {
    local dir="$1"
    local ignore_not_exist="${2:-}"
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

for f in \
    /usr/lib64/openmpi/bin \
    /usr/local/cuda/bin \
    $(ls -td /usr/java/{jdk*,jre*} /opt/IBMJava* 2>/dev/null) \
    /usr/local/bin \
    /opt/local/bin \
    $( [[ $EUID == 0 ]] && echo /sbin /usr/sbin /usr/local/sbin /opt/local/sbin) \
    "$HOME"/bin \
    ; do
    bivio_path_insert "$f"
done

f=/usr/lib64/openmpi/lib
if [[ -d $f && ! ( :$LD_LIBRARY_PATH: =~ :$f: ) ]]; then
    export LD_LIBRARY_PATH=$f${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
fi
unset f

if bivio_in_docker; then
    # https://github.com/radiasoft/devops/issues/132
    # https://github.com/open-mpi/ompi/issues/3270
    export OMPI_MCA_btl=self,sm,tcp
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

export FTP_PASSIVE=1

if [[ -f "$HOME"/.ssh/ssh_agent ]]; then
    . "$HOME"/.ssh/ssh_agent > /dev/null
    if [[ -n "$PS1" ]]; then
        if ! ps ${SSH_AGENT_PID-0} 2>&1 | grep -s -q ssh-agent; then
	    # Start a daemon and add
	    ssh-agent > "$HOME"/.ssh/ssh_agent
	    . "$HOME"/.ssh/ssh_agent
	    ssh-add
            x="$HOME"/.vagrant.d/insecure_private_key
            if [[ -f $x  ]]; then
                ssh-add $x
            fi
            unset x
	fi
    fi
fi

#POSIT: duplicate code in install.sh
if [[ -z ${BIVIO_WANT_PERL+x} ]]; then
    # If someone set these vars, they expect to do perl development
    # or only on centos if GMP for perl is installed (only if BOP is installed).
    if [[ -n ${BIVIO_HTTPD_PORT+x} || -n ${BIVIO_HOST_NAME+x} ]]; then
        export BIVIO_WANT_PERL=1
    else
        export BIVIO_WANT_PERL=
    fi

fi

if [[ -n $BIVIO_WANT_PERL ]]; then
    if [[ -z $PERLLIB && -d "$HOME"/src/perl ]]; then
        export PERLLIB=$HOME/src/perl
    fi

    if bivio class info Bivio::BConf >& /dev/null; then
        b() {
            bivio "$@"
        }

        if [[ -d "$HOME"/src/biviosoftware/perl-Bivio ]]; then
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

radia_run() {
    local u=${install_server:-}
    if [[ ! $u || $u == github ]]; then
        u=https://depot.radiasoft.org
    fi
    curl -s -S -L "$u/index.sh" | bash -l -s "$@"
}

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
