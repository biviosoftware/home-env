#!/bin/bash
#
# curl -s -S -L ${BIVIO_GIT_SERVER-https://raw.githubusercontent.com}/biviosoftware/home-env/master/bin/install.sh | bash
# For development, do this:
#
biviosoftware=~/src/biviosoftware

is_cygwin=
if [[ $(uname) =~ CYGWIN ]]; then
    is_cygwin=1
fi

mkdir -p ~/bin

mkdir -p "$biviosoftware"
cd "$biviosoftware"

for repo in home-env \
    $( [[ $want_perl ]] && echo perl-Bivio javascript-Bivio perl-ProjEx ) \
    ; do
    if [[ -d $repo ]]; then
        (
            cd "$repo"
            git pull -q
        )
    else
        git clone -q "${BIVIO_GIT_SERVER-https://github.com}/biviosoftware/$repo.git"
    fi
done

# Check for dead links from previous install before this install in case
# we moved a file
for f in ~/.??* ~/bin/*; do
    if [[ -L $f && ! -e $f ]]; then
        rm "$f"
    fi
done

if [[ $want_perl ]]; then
    mkdir -p ../perl

    for p in Bivio ProjEx; do
        if [[ -L ../perl/$p ]]; then
            rm "../perl/$p"
        fi
        ln -s "../biviosoftware/perl-$p" "../perl/$p"
    done

    for p in bivio b-env; do
        if [[ ! -x ~/bin/$p ]]; then
            ln -s ~/src/biviosoftware/perl-Bivio/Util/"$p" ~/bin/"$p"
            chmod +x ~/bin/$p
        fi
    done
fi

cd home-env

# Our file names don't have spaces so ok
shopt -s nullglob
for f in {bin,dot}/*; do
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
        dst=~/$f
    else
        dst=~/.${f#dot/}
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
    dst=~/.$f
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

cd

# npmrc may contain credentials so need to append
if grep -q -s '^color' .npmrc; then
    : ok
else
    echo 'color = false' >> .npmrc
    chmod 600 .npmrc
fi
