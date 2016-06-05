if bivio_not_src_home_env; then
    return
fi

if [[ $PS1 && -t 0 ]] && shopt -q login_shell && _bivio_home_env_update; then
    echo "Sourcing: ~/.bashrc" 1>&2
    . ~/.bashrc
    return
fi


export BIVIO_HTTPD_PORT=${BIVIO_HTTPD_PORT:-$((8000 + $(id -u) * 2 % 100))}
export BIVIO_IS_2014STYLE=${BIVIO_IS_2014STYLE:-0}

# Avoid "Error: DEPTH_ZERO_SELF_SIGNED_CERT" from Node.js
export NODE_TLS_REJECT_UNAUTHORIZED=0

if [[ -z $BIVIO_HOST_NAME ]]; then
    if [[ $HOSTNAME == apa3.bivio.biz ]]; then
        BIVIO_HOST_NAME=dev.bivio.biz
    elif type -t ifconfig &> /dev/null; then
	eval $(ifconfig | perl -ne '/addr:10\.10\.10\.(\d+)/ && print(qq{BIVIO_HOST_NAME=z$1.bivio.biz})')
	if [[ -z $BIVIO_HOST_NAME ]]; then
	    BIVIO_HOST_NAME=$(hostname)
	fi
    fi
    export BIVIO_HOST_NAME
fi

if type -t bu &>/dev/null; then
    if [[ -z $BIVIO_DEFAULT_BCONF ]]; then
        for x in Artisans::BConf Bivio::DefaultBConf; do
            if perl -M$x -e 1 &>/dev/null; then
                export BIVIO_DEFAULT_BCONF=$x
                break
            fi
        done
    fi
    if [[ $BIVIO_DEFAULT_BCONF ]]; then
        eval "$(env BCONF=$BIVIO_DEFAULT_BCONF bivio dev bashrc_b_env_aliases)"

        #TODO(robnagler): backwards compatibility for bashrc_b_env_aliases
        b_ps1() {
            bivio_ps1 "$@"
        }
        if b_env pet Bivio/PetShop; then
            cd - &> /dev/null
        fi
    fi
fi

if [[ -d ~/.pyenv/bin ]]; then
    bivio_path_insert ~/.pyenv/bin
    if [[ function != $(type -t pyenv) ]]; then
        eval "$(pyenv init -)"
        # pyenv init always inserts shims in the path
        bivio_path_dedup
    fi
    if [[ function != $(type -t _pyenv_virtualenv_hook) ]]; then
        eval "$(pyenv virtualenv-init -)"
    fi
    if [[ $PS1 ]]; then
        bivio_pyenv_virtualenv_hook() {
            if [[ function != $(type -t pyenv) || ! -x ~/.pyenv/bin/pyenv ]]; then
                export PROMPT_COMMAND=bivio_prompt_command
                return
            fi
            _pyenv_virtualenv_hook
            if [[ -z $VIRTUAL_ENV ]]; then
                bivio_ps1 $(pyenv global)
            else
                bivio_ps1 $(basename "$VIRTUAL_ENV")
            fi
            bivio_prompt_command
        }
        export PROMPT_COMMAND=bivio_pyenv_virtualenv_hook
    fi
fi

_bivio_pyenv_source() {
    local source=$1
    # Avoid recursion
    if [[ $_bivio_pyenv_source_stack ]]; then
        if [[ $_bivio_pyenv_source_stack[@] =~ $source ]]; then
            return
        fi
    else
        local _bivio_pyenv_source_stack=()
    fi
    _bivio_pyenv_source_stack+=($source)
    (
        set -e
        . $source
    )
    local res=$?
    _bivio_pyenv_source_stack=${_bivio_pyenv_source_stack[@]/$source/}
    if [[ $res != 0 ]]; then
        echo 'ERROR: install failed' 1>&2
    fi
    return $res
}

bivio_pyenv_deactivate() {
    if [[ $PYENV_ACTIVATE ]]; then
        # so safe to deactivate any time
        pyenv deactivate || true
    fi
    # This needs to be cleared for auto-de/activation to work again
    unset VIRTUAL_ENV
    # Remove global version
    rm -f ~/.pyenv/version
}

bivio_pyenv_global() {
    local _bivio_pyenv_global_version=$1
    bivio_pyenv_deactivate
    _bivio_pyenv_source _bivio_pyenv_global
}

bivio_pyenv_local() {
    _bivio_pyenv_source _bivio_pyenv_local
}

bivio_pyenv_2() {
    bivio_pyenv_global 2.7.10
}

bivio_pyenv_3() {
    bivio_pyenv_global 3.4.3
}

gcl() {
    local r=$1
    if ! [[ $r =~ / ]]; then
	r="$(basename $(pwd))/$r"
    fi
    if ! [[ $r =~ ^[a-z]+:/ ]]; then
        r="https://github.com/$r"
    fi
    git clone "$r"
}

gchmod() {
    git update-index --chmod=+x "$@"
}

ghead() {
    git checkout HEAD "$@"
}

gpu() {
    git push origin master "$@"
    git push --tags
}

gst() {
    git status -s "$@"
}

gtag() {
    local tag=$1
    git tag -d "$tag"
    git push origin :refs/tags/"$tag"
    git tag "$tag"
    git push --tags
}

gup() {
    git pull "$@"
    git fetch --tags
}

http() {
    python2 -m SimpleHTTPServer "$BIVIO_HTTPD_PORT"
}

mocha() {
    command mocha "$@" | perl -p -e 's/\e.*?m//g'
}

nup() {
    cvs -n up 2>/dev/null|egrep '^[A-Z] |^\? .*\.(pm|bview|gif|jpg|t|PL|btest|bunit|bconf|msg|css|js|png|psd|pdf|spec|xml|java)$'
}

up() {
    cvs up -Pd
}

vssh() {
    bivio_vagrant_ssh "$@"
}
