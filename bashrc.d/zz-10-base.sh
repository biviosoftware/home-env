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

if [[ ! ${bivio_color:-} ]]; then
    # Undo some stuff
    for f in $(compgen -a); do
        unalias "$f"
    done
    unset f
fi

# Undo bivio functions from /etc/bashrc.d
for f in bconf b bu ba bi bihs ctd g gp $(compgen -A function | egrep '^(b_|bivio_)'); do
    if [[ $f != bivio_not_src_home_env ]]; then
        unset -f "$f"
    fi
done
unset f

umask "${BIVIO_UMASK:-g-w,o-rwx}"
if [[ ! ${bivio_color:-} ]]; then
    export LS_COLORS=
    export USER_LS_COLORS=
    export PROMPT_COMMAND=
    export VAGRANT_NO_COLOR=true
fi
export USER=${USER:-$(id -u -n)}
export LOGNAME=${LOGNAME:-$(logname 2>/dev/null || echo $USER)}
unset BCONF
# Darwin (Catalina onwards replaced bash with zsh)
export BASH_SILENCE_DEPRECATION_WARNING=1

# python pip installs in /tmp, which doesn't work if the package is large
# and /tmp is on tmpfs.
if [[ ! ${TMPDIR:-} && $(df /tmp 2>&1 | tail -1) =~ tmpfs ]]; then
    export TMPDIR=${TMPDIR-/var/tmp}
fi

if [[ ! ${TZ:-} && -e /etc/localtime ]]; then
    # Tested on CentOS 7, and it does have the localtime stat problem
    # https://blog.packagecloud.io/eng/2017/02/21/set-environment-variable-save-thousands-of-system-calls/
    export TZ=:/etc/localtime
fi

if [[ ${TERM:-} =~ ^(screen.xterm-256color|screen|screen256|screen-256color)$ && -r /usr/share/terminfo/x/xterm-256color-screen ]]; then
    export TERM=xterm-256color-screen
fi

bivio_not_strict_cmd() {
    local flags=$-
    set +eu
    "$@"
    if [[ $flags =~ e ]]; then
        set -e
    fi
    if [[ $flags =~ u ]]; then
        set -u
    fi
}

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
    if [[ ! ${PS1:-} ]]; then
        return
    fi
    local x='['
    if [[ ${1+1} ]]; then
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

if [[ ${PS1:-} ]]; then
    bivio_prompt_command() {
        case ${TERM:-} in
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
    if [[ ${EUID:-} == 0 ]]; then
        bivio_ps1_suffix='# '
    else
        bivio_ps1_suffix='$ '
    fi
    if [[ ${TERM:-} != dumb ]]; then
        stty quit '^_'
    fi
    PS1="\W$bivio_ps1_suffix"
    PROMPT_COMMAND=bivio_prompt_command
    bivio_prompt_command
fi

bivio_classpath_append() {
    local jar="$1"
    if [[ ! ( :${CLASSPATH:-}: =~ :$jar: ) ]]; then
	export CLASSPATH=${CLASSPATH:-}${CLASSPATH+:}$jar
    fi
}

bivio_ld_library_path_append() {
    local dir="$1"
    local ignore_not_exist="${2:-}"
    if [[ ( $ignore_not_exist || -d $dir ) && ! ( :$LD_LIBRARY_PATH: =~ :$dir: ) ]]; then
	export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$dir"
    fi
}

bivio_ld_library_path_remove() {
    export LD_LIBRARY_PATH=$(perl -e 'print(join(q{:}, grep($_ ne $ARGV[0], split(/:/, $ENV{LD_LIBRARY_PATH}))))' "$1")
}

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

# Give precedence to NERSC's shifter, but /opt/udiImage/modules/mpich/bin doesn't
# seem to exist so probably moot. More important point is LD_LIBRARY_PATH (below)
for f in \
    /usr/lib64/openmpi/bin \
    /usr/lib64/mpich/bin \
    /usr/local/cuda/bin \
    $(ls -td /usr/java/{jdk*,jre*} /opt/IBMJava* 2>/dev/null || true) \
    /usr/local/bin \
    /opt/local/bin \
    $( [[ ${EUID:-} == 0 ]] && echo /sbin /usr/sbin /usr/local/sbin /opt/local/sbin ) \
    "$HOME"/bin \
    "$HOME"/.local/bin \
    /opt/udiImage/modules/mpich/bin \
    ; do
    bivio_path_insert "$f"
done

# POSIT: BIVIO_MPI_LIB is only used for compiling
# See https://github.com/radiasoft/download
unset BIVIO_MPI_LIB
for f in \
    /usr/lib64/mpich/lib \
    /usr/lib64/openmpi/lib \
    /usr/local/lib \
    ; do
    if [[ $(shopt -s nullglob && echo $f/libmpi.so*) ]]; then
        export BIVIO_MPI_LIB=$f
        break
    fi
done
if [[ ${SHIFTER_RUNTIME:-} ]]; then
    if [[ ! ${LD_LIBRARY_PATH:-} ]]; then
        echo "ERROR: LD_LIBRARY_PATH is empty in Shifter; see ${BASH_SOURCE[0]}" 1>&2
    elif [[ ! ${SLURM_JOB_ID:-} ]]; then
        # Inside Shifter at NERSC. On a login node, we have no MPI, so we want to clear
        # LD_LIBRARY_PATH so programs don't crash which import mpi.
        # See https://github.com/biviosoftware/home-env/issues/49.
        export LD_LIBRARY_PATH=
    fi
elif [[ ${BIVIO_MPI_LIB:-} ]]; then
     bivio_ld_library_path_append "$BIVIO_MPI_LIB"
fi
# RadiaSoft libraries which do not take precedence over Shifter or MPI
bivio_ld_library_path_append $HOME/.local/lib

# Used by RadiaSoft (RADIA) staff. Works on NERSC or JupyterHub
if [[ ! ${RADIA_SCRATCH:-} ]]; then
    if [[ ${SCRATCH:-} && -d $SCRATCH && -w $SCRATCH ]]; then
        export RADIA_SCRATCH=$SCRATCH
    elif [[ ${JUPYTERHUB_USER:-} ]]; then
        f=$HOME/jupyter/StaffScratch/$JUPYTERHUB_USER
        if [[ -d $f && -w $f ]]; then
            export RADIA_SCRATCH=$f
        fi
    fi
fi

if [[ ! ${HISTFILE:-} ]]; then
    f="$HOME"/jupyter/.bash_history
    if [[ -w $f ]]; then
        export HISTFILE=$f
    fi
fi
unset f

export PKG_CONFIG_PATH="$HOME/.local/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"

if bivio_in_docker && [[ ${BIVIO_MPI_PREFIX:-} =~ openmpi ]]; then
    # https://github.com/radiasoft/devops/issues/132
    # https://github.com/open-mpi/ompi/issues/3270
    export OMPI_MCA_btl=self,sm,tcp
fi

export JAVA_HOME=/usr/lib/jvm/java
#java -cp .:/usr/share/java/junit.jar org.junit.runner.JUnitCore
# Get most recent java and any jars in /usr/java
for f in $(ls "$HOME"/.local/share/java/*.jar /usr/java/*.jar 2> /dev/null || true); do
    bivio_classpath_append $f
done
unset f

if [[ -d $HOME/src/java ]]; then
    export JAVA_ROOT=$HOME/src/java
    bivio_classpath_append "$JAVA_ROOT"
fi

export FTP_PASSIVE=1

if [[ -f $HOME/.ssh/ssh_agent ]]; then
    source "$HOME"/.ssh/ssh_agent > /dev/null
    if [[ ${PS1:-} ]]; then
        if ! ps ${SSH_AGENT_PID-0} 2>&1 | grep -s -q ssh-agent; then
	    # Start a daemon and add
	    ssh-agent > "$HOME"/.ssh/ssh_agent
	    source "$HOME"/.ssh/ssh_agent
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

if [[ ${BIVIO_WANT_PERL:-} ]]; then
    if [[ ! ${PERLLIB:-} && -d "$HOME"/src/perl ]]; then
        export PERLLIB=$HOME/src/perl
    fi
    if bivio class info Bivio::BConf >& /dev/null; then
        b() {
            bivio "$@"
        }

        if [[ -d $HOME/src/biviosoftware/perl-Bivio ]]; then
            bu() {
                bivio test unit "${@-.}"
            }

            ba() {
                bivio test acceptance "${@-.}"
            }
        fi
    fi
fi

g() {
    local x="$1"
    shift
    grep -iIr --exclude-dir=.git --exclude='*~' --exclude='.#*' --exclude='*/.#*' \
        "$x" "${@-.}" 2>/dev/null
}

function gp() {
    local x="$1"
    shift
    # --include must be first
    grep -iIr \
        --include '*.PL' \
        --include '*.btest' \
        --include '*.bunit' \
        --include '*.pl' \
        --include '*.pm' \
        --include '*.py' \
        --include '*.t' \
        --exclude-dir=.git \
        --exclude-dir=\*.tmp \
        --exclude-dir=files/artisans/plain/f/bOP \
        --exclude-dir=old \
        --exclude-dir=tmp \
        --exclude='*/.#*'\
        --exclude='*~' \
        --exclude='.#*' \
        --exclude=bOP.pm \
        "$x" "${@-.}" |
	egrep -v '/files/artisans/plain/f/bOP'
}

radia_run() {
    local u=${install_server:-}
    if [[ ! $u || $u == github ]]; then
        u=https://depot.radiasoft.org
    fi
    curl -s -S -L "$u/index.sh" | bash -l -s "$@"
}

if [[ $(type -p emacs) && $(type -p emacsclient) ]]; then
    if [[ ${INSIDE_EMACS:-} =~ comint ]]; then
        # It's probably dumb, but force to be sure
        export TERM=dumb
        # older emacs used "dirs" now emacs uses "command dirs"
        unset dirs

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
    if [[ ${TERM:-} == dumb ]]; then
        export EDITOR=$(type -p emacsclient)
        export NODE_NO_READLINE=1
        export PAGER=$(type -p cat)
        export SYSTEMD_COLOR=0
    fi
fi

if [[ ! ${bivio_color:-} ]]; then
    function which() {
        type "$@"
    }
fi

clean() {
    find . -name '*~' -exec rm {} \;
}
