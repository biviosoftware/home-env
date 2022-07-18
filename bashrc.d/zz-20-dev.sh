if bivio_not_src_home_env; then
    return
fi

#TODO(robnagler) I don't think this is needed any more
# Avoid "Error: DEPTH_ZERO_SELF_SIGNED_CERT" from Node.js
# export NODE_TLS_REJECT_UNAUTHORIZED=0

if [[ ${BIVIO_WANT_PERL:-} ]]; then
    export BIVIO_HTTPD_PORT=${BIVIO_HTTPD_PORT:-$((8000 + $(id -u) * 2 % 100))}
    if [[ ! ${BIVIO_HOST_NAME:-} ]]; then
	export BIVIO_HOST_NAME=$(hostname)
    fi
    if type -t bu &>/dev/null; then
        if [[ ! ${BIVIO_DEFAULT_BCONF:-} ]]; then
            for x in Artisans::BConf Bivio::DefaultBConf; do
                if perl -M"$x" -e 1 &>/dev/null; then
                    export BIVIO_DEFAULT_BCONF=$x
                    break
                fi
            done
            unset x
        fi
        if [[ ${BIVIO_DEFAULT_BCONF:-} ]]; then
            eval "$(env BCONF=$BIVIO_DEFAULT_BCONF bivio dev bashrc_b_env_aliases)"
            if b_env pet Bivio/PetShop; then
                cd - &> /dev/null
            fi
        fi
    fi
fi

if [[ ${PS1:-} && -t 0 ]] && shopt -q login_shell && _bivio_home_env_update; then
    echo "Sourcing: $HOME/.bashrc" 1>&2
    bivio_not_strict_cmd source "$HOME"/.bashrc
    return
fi

# always set PYENV_ROOT
export PYENV_ROOT=$HOME/.pyenv
if [[ -d $PYENV_ROOT/bin ]]; then
    # Avoid warning "prompt changing will be removed from future release"
    export PYENV_VIRTUALENV_DISABLE_PROMPT=1
    bivio_path_insert "$PYENV_ROOT"/bin
    if [[ function != $(type -t pyenv || true) ]]; then
        _no_rehash=
        # PERFORMANCE: rehash takes .5s, and sometimes it hangs on
        # writing to a file: ~/.pyenv/shims/.pyenv-shim. If we don't
        # have an interactive shell, don't rehash.
        if [[ ! -w $PYENV_ROOT/shims || ! ${PS1:-} ]]; then
            # If we can't update shims, then can't rehash (see download/installers/container-run)
            _no_rehash=--no-rehash
        fi
        if [[ $(pyenv --version) =~ pyenv.1 ]]; then
            eval "$(pyenv init - $_no_rehash)"
            # pyenv init always inserts shims in the path
            bivio_path_dedup
        else
            # pyenv 2 or greater
            # simulation pyenv init --path, because it doesn't dedup, and bivio_path_insert does
            # plus avoids WARNING: pyenv init -` no longer sets PATH
            bivio_path_insert "$PYENV_ROOT"/shims
            eval "$(pyenv init - $_no_rehash)"
        fi
        unset _no_rehash
    fi
    if [[ function != $(type -t _pyenv_virtualenv_hook) ]]; then
        eval "$(pyenv virtualenv-init -)"
    fi
    if [[ ${PS1:-} ]]; then
        bivio_pyenv_virtualenv_hook() {
            if [[ function != $(type -t pyenv) || ! -x $PYENV_ROOT/bin/pyenv ]]; then
                export PROMPT_COMMAND=bivio_prompt_command
                return
            fi
            _pyenv_virtualenv_hook
            if [[ ! ${VIRTUAL_ENV:-} ]]; then
                bivio_ps1 $(pyenv global)
            else
                bivio_ps1 $(basename "$VIRTUAL_ENV")
            fi
            bivio_prompt_command
        }
        export PROMPT_COMMAND=bivio_pyenv_virtualenv_hook
    fi
fi

bivio_path_insert "$HOME/brew/bin"
if [[ ! ${bivio_color:-} && $(type -p brew) ]]; then
    export HOMEBREW_NO_EMOJI=1
    export HOMEBREW_NO_COLOR=1
fi

bivio_brew_install() {
    if [[ $(uname) != Darwin ]]; then
        echo "Brew is only needed on Mac OS" 1>&2
        return 1
    fi
    if [[ ! $(type -p git) ]]; then
        echo 'You need to install Xcode. Run:
xcode-select --install
' 1>&2
        return 1
    fi
    local d="$HOME/brew"
    if ! mkdir "$d"; then
        echo "$d already exists; homebrew is already installed" 1>&2
        return 1
    fi
    curl -L -s -S https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C "$d"
    bivio_not_strict_cmd source "$HOME"/.bashrc
    (
        eval "$("$d"/bin/brew shellenv)"
        brew update --force --quiet
    )
}

_bivio_pyenv_source() {
    local source=$1
    # Avoid recursion
    if [[ ${_bivio_pyenv_source_stack:-} ]]; then
        if [[ $_bivio_pyenv_source_stack[@] =~ $source ]]; then
            return
        fi
    else
        local _bivio_pyenv_source_stack=()
    fi
    _bivio_pyenv_source_stack+=($source)
    (
        set -e
        source $source
    )
    local res=$?
    _bivio_pyenv_source_stack=${_bivio_pyenv_source_stack[@]/$source/}
    if [[ $res != 0 ]]; then
        echo 'ERROR: install failed' 1>&2
    fi
    return $res
}

_bivio_pyenv_version() {
    bivio_not_strict_cmd _bivio_pyenv_version_do "$@"
}

_bivio_pyenv_version_do() {
    local v=$1
    local ve=$2
    # This line stops a warning from the pyenv installer
    bivio_path_insert "$PYENV_ROOT"/bin 1
    bivio_not_strict_cmd source "$HOME"/.bashrc
    bivio_not_strict_cmd bivio_pyenv_global "$v"
    bivio_not_strict_cmd source "$HOME"/.bashrc
    pip install --upgrade pip
    pip install --upgrade setuptools tox
    pyenv virtualenv "$v" "$ve"
    pyenv global "$ve"
}

bivio_pyenv_deactivate() {
    if [[ ${PYENV_ACTIVATE:-} ]]; then
        # so safe to deactivate any time
        pyenv deactivate || true
    fi
    # This needs to be cleared for auto-de/activation to work again
    unset VIRTUAL_ENV
    # Remove global version
    rm -f "$PYENV_ROOT"/version
}

bivio_pyenv_global() {
    local _bivio_pyenv_global_version=$1
    bivio_pyenv_deactivate
    _bivio_pyenv_source _bivio_pyenv_global
}

bivio_pyenv_local() {
    _bivio_pyenv_source _bivio_pyenv_local
}

bivio_pyenv_3() {
    local v=3.7.2
    if [[ $(uname) == Darwin ]]; then
        local x=$(sw_vers -productVersion)
        x=( ${x//./} )
        if (( ${x[0]} >= 12 )); then
            # Monterey doesn't compile 3.7.2
            # https://github.com/pyenv/pyenv/issues/2143#issuecomment-1072032647
            v=3.10.3
        fi
    fi
    _bivio_pyenv_version "$v" py3
}

gcl() {
    local r=$1
    if ! [[ $r =~ / ]]; then
	r=$(basename "$(pwd)")/$r
    fi
    if ! [[ $r =~ ^[a-z]+:/ ]]; then
        r=https://github.com/$r
    fi
    local b=$(basename "$r" .git)
    if [[ -r $b ]]; then
        (cd $b && git pull)
    else
        git clone "$r"
    fi
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
    python3 -m http.server "${BIVIO_HTTPD_PORT:-8000}"
}

mocha() {
    command mocha "$@" | perl -p -e 's/\e.*?m//g'
}

vssh() {
    if grep '^ID=ubuntu' /etc/os-release >& /dev/null; then
        if ! vagrant status 2>&1 | grep '^default.*running' >& /dev/null; then
            vagrant up
        fi
        vagrant ssh ${*:+-c "$*"}
    else
        bivio_vagrant_ssh "$@"
    fi
}

for f in $(shopt -s nullglob; echo "$HOME"/.local/etc/bashrc.d/*.sh); do
    bivio_not_strict_cmd source "$f"
done
