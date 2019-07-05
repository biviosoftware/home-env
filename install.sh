#!/bin/bash
#
# radia_run home
#
# curl radia.run | bash -s home
#
# See https://github.com/radiasoft/download for development
#
#
set -euo pipefail
if [[ -r "$HOME"/.pre_bivio_bashrc ]]; then
    set +euo pipefail
    source "$HOME"/.pre_bivio_bashrc
    set -euo pipefail
fi

#POSIT: duplicate code in zz-10-base.sh
if [[ ! ${BIVIO_WANT_PERL+x} ]]; then
    # See zz-10-base.sh
    if [[ ${BIVIO_HTTPD_PORT+x} || ${BIVIO_HOST_NAME+x} ]]; then
        export BIVIO_WANT_PERL=1
    else
        export BIVIO_WANT_PERL=
    fi

fi

biviosoftware="$HOME"/src/biviosoftware

is_cygwin=
if [[ $(uname) =~ CYGWIN ]]; then
    is_cygwin=1
fi

mkdir -p "$HOME"/bin
mkdir -p "$biviosoftware"
cd "$biviosoftware"

for repo in home-env \
    $( [[ $BIVIO_WANT_PERL ]] && echo perl-Bivio javascript-Bivio perl-ProjEx ) \
    ; do
    if [[ -d $repo ]]; then
        (
            cd "$repo"
            git pull -q
        )
    else
        u=https://github.com/biviosoftware/$repo.git
        # For development
        if [[ ${home_env_dev:-} && ${install_server:-} =~ 2916 ]]; then
            # NOTE: requires install
            u=$install_server/biviosoftware/$repo/.git
        fi
        git clone -q "$u"

    fi
done

# Check for dead links from previous install before this install in case
# we moved a file
for f in "$HOME"/.??* "$HOME"/bin/*; do
    if [[ -L $f && ! -e $f ]]; then
        rm "$f"
    fi
done

if [[ $BIVIO_WANT_PERL ]]; then
    mkdir -p ../perl
    for p in Bivio ProjEx; do
        if [[ -L ../perl/$p ]]; then
            rm "../perl/$p"
        fi
        ln -s "../biviosoftware/perl-$p" "../perl/$p"
    done

    for p in bivio b-env b-sendmail-http; do
        if [[ ! -x "$HOME"/bin/$p ]]; then
            ln -s "$HOME"/src/biviosoftware/perl-Bivio/Util/"$p" "$HOME"/bin/"$p"
            chmod +x "$HOME"/bin/$p
        fi
    done
fi

cd home-env

# Our file names don't have spaces so ok
for f in $(shopt -s nullglob; echo {bin,dot}/*); do
    src=$PWD/$f
    if [[ ! $(basename $src) =~ ^[_a-zA-Z][_a-zA-Z0-9-]+$ ]]; then
        # probably tilde or backup file
        continue
    fi
    cmd='ln -s'
    if [[ $is_cygwin && -r cygwin/$f ]]; then
        src=$PWD/cygwin/$f
        cmd='cp -a'
    fi
    if [[ $src =~ bin/ ]]; then
        chmod +x "$src"
        dst="$HOME"/$f
    else
        dst="$HOME"/.${f#dot/}
    fi
    if [ -e "$dst" ]; then
        if cmp --silent "$dst" "$src"; then
            continue
        fi
        rm -f "$dst.old"
	mv "$dst" "$dst.old"
    fi
    $cmd "$src" "$dst"
done

for f in gitconfig netrc; do
    dst="$HOME"/.$f
    if [[ -e $dst ]]; then
        continue
    fi
    if [[ -L $dst ]]; then
        rm "$dst"
    fi
    src=$PWD/template/$f
    if [[ -r $src ]]; then
        echo "Copying template to $dst; YOU NEED TO EDIT IT."
	install -m 0600 "$src" "$dst"
    fi
done

docker_config="$HOME"/.docker/config.json
if [[ ! -r $docker_config ]]; then
    install -d -m 700 "$(dirname "$docker_config")"
    install -m 600 $PWD/template/docker-config.json "$docker_config"
fi

cd

# npmrc may contain credentials so need to append
if ! grep -q -s '^color' .npmrc; then
    echo 'color = false' >> .npmrc
    chmod 600 .npmrc
fi

# append detachKeys if not there
if ! grep -q -s detachKeys "$docker_config"; then
    perl -pi -e 's/(?<=^\{)/\n  "detachKeys": "ctrl-],q",/' "$docker_config"
fi

if [[ -n $BIVIO_WANT_PERL && ! -d "$HOME"/btest-mail ]]; then
    (
        set +euo pipefail
        source "$HOME"/.bashrc
        # might error out, because we don't have a db yet
        bivio dev setup
        bivio project link_facade_files
        true
    ) >& /dev/null
fi
