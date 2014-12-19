if [ $(expr match "$BASH_SOURCE" ~/src) = 0 -a -d ~/src/biviosoftware/home-env ]; then
    # Execute user's dot files only
    return
fi

b_install_nvm() {
    if [ ! -d ~/.nvm ]; then
	curl -L https://raw.githubusercontent.com/creationix/nvm/master/install.sh | env PROFILE=/dev/null bash
    fi
    nvm install stable
}

b_nvm() {
    local v="$1"
    case "x$v" in
	xstable)
	    ;;
	*)
	    echo 'You need to supply a version (e.g. stable)' 1>&2
	    return 1
	    ;;
    esac
    if [ -z "$NVM_DIR" ]; then
	export NVM_DIR=~/.nvm
	test -s "$NVM_DIR/nvm.sh" && . "$NVM_DIR/nvm.sh"
	local d
	for d in ../../.. ../.. .. .; do
	    b_insert_path $d/node_modules
	done
    fi
    nvm use $v
    if [ -x /usr/bin/node ]; then
	echo &> 'Remove global node and npm:'
        echo &> 'yum remove npm node'
    fi
}

b_pyenv() {
    local v="$1"
    export WORKON_HOME=$HOME/Envs
    export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV=true
    b_path_insert "$HOME/.pyenv/bin"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
    pyenv virtualenvwrapper
    if [ ! -z "$v" -a -d $WORKON_HOME ]; then
	workon py$v
	b_ps1 py$v
    fi
}

b_install_pyenv() {
    local v="$1"
    local vv
    case "x$v" in
	x3)
	    vv=3.4.2
	    ;;
	x2)
	    vv=2.7.8
	    ;;
	*)
	    echo 'You need to supply a python version' 1>&2
	    return 1
	    ;;
    esac
    if [ ! -d ~/.pyenv ]; then
	curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
    fi
    b_pyenv $v 2>/dev/null
    pyenv install $vv
    if [ $v = 3 ]; then
	pyenv global $vv
    fi
    pip install virtualenvwrapper
    if [ ! -d ~/.pyenv/plugins/pyenv-virtualenvwrapper ]; then
	git clone https://github.com/yyuu/pyenv-virtualenvwrapper.git ~/.pyenv/plugins/pyenv-virtualenvwrapper
    fi
    b_pyenv $v 2>/dev/null
    pyenv virtualenvwrapper
    mkvirtualenv py$v
    b_pyenv $v
}

gcl() {
    local r=$1
    if [ $(expr match "$r" '.*/') = 0 ]; then
	r=$(basename $(pwd))/$r
    fi
    git clone "https://github.com/$r"
}

ctd() {
    perl <<'EOF' "$@"
    system('bivio project link_facade_files') 
        if -M "$ENV{PERLLIB}/../javascript/qooxdoo/b_agent/build/index.html" > 1;
    exec(
	qw(
	    bivio
	    SQL
	    -force
	    create_test_db
	    --Bivio::IO::Trace.package_filter=
	    --Bivio::Die.stack_trace=0
	    --Bivio::Die.stack_trace_error=0
	    --Bivio::IO::Alert.stack_trace_warn=0
	),
	@ARGV,
    );
EOF
}

mocha() {
    command mocha "$@" | perl -p -e 's/\e.*?m//g'
}

export BIVIO_HTTPD_PORT=${BIVIO_HTTPD_PORT:-$(perl -e 'printf(q{80%02d}, (`id -u` =~ /(\d+)/)[0] * 2 % 100)')}
export BIVIO_IS_2014STYLE=${BIVIO_IS_2014STYLE:-0}


if [ -z "$BIVIO_HOST_NAME" ]; then
    if [ "x$(hostname)" = xapa3.bivio.biz ]; then
        BIVIO_HOST_NAME=dev.bivio.biz
    else
	eval $(ifconfig | perl -ne '/addr:10\.10\.10\.(\d+)/ && print(qq{BIVIO_HOST_NAME=z$1.bivio.biz})')
	if [ -z "$BIVIO_HOST_NAME" ]; then
	    BIVIO_HOST_NAME=$(hostname)
	fi
    fi
    export BIVIO_HOST_NAME
fi

if [ -z "$BIVIO_CFG_DIR" ]; then
    if [ -d /vagrant ]; then
        BIVIO_CFG_DIR=/vagrant
    fi
    export BIVIO_CFG_DIR
fi

if [ -z "$BIVIO_DEFAULT_BCONF" ]; then
    for x in Artisans::BConf Bivio::DefaultBConf; do
        if perl -M$x -e 1 2>/dev/null; then
	    export BIVIO_DEFAULT_BCONF=$x
	    break
        fi
    done
fi

if [ ! -z "$BIVIO_DEFAULT_BCONF" ]; then
    eval "$(env BCONF=$BIVIO_DEFAULT_BCONF bivio dev bashrc_b_env_aliases)"
    b_env pet Bivio/PetShop && cd - > /dev/null
fi

# Avoid "Error: DEPTH_ZERO_SELF_SIGNED_CERT" from Node.js
export NODE_TLS_REJECT_UNAUTHORIZED=0

alias gco='git commit -am'
alias gpu='git push origin master'
alias gst='git status'
alias gup='git pull'
alias nup="cvs -n up 2>/dev/null|egrep '^[A-Z] |^\\? .*\\.(pm|bview|gif|jpg|t|PL|btest|bunit|bconf|msg|css|js|png|psd|pdf|spec|xml|java)$'"
alias py2='b_pyenv 2'
alias py3='b_pyenv 3'
alias up="cvs up -Pd"
