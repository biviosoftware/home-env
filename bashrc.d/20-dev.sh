# -*-sh-*-
if [ $(expr match "$BASH_SOURCE" ~/src) == 0 -a -d ~/src/biviosoftware/home-env ]; then
    # Execute user's dot files only
    return
fi
export BIVIO_HTTPD_PORT=${BIVIO_HTTPD_PORT:-$(perl -e 'printf(q{80%02d}, (`id -u` =~ /(\d+)/)[0] % 100)')}
export BIVIO_IS_2014STYLE=${BIVIO_IS_2014STYLE:-0}

if [ -z "$BIVIO_HOST_NAME" -a "x$(hostname)" = xapa3.bivio.biz ]; then
    export BIVIO_HOST_NAME=dev.bivio.biz
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

function gcl {
    local r=$1
    if expr "$r" : '.*/' >/dev/null; then
	r=$(basename $(pwd))/$r
    fi
    git clone "https://github.com/$r"
}

function ctd {
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

alias gup='git pull'
alias gpu='git push origin master'
alias gco='git commit -am'
alias gst='git status'
alias nup="cvs -n up 2>/dev/null|egrep '^[A-Z] |^\\? .*\\.(pm|bview|gif|jpg|t|PL|btest|bunit|bconf|msg|css|js|png|psd|pdf|spec|xml|java)$'"
alias up="cvs up -Pd"
