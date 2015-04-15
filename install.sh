#!/bin/bash
# curl -L https://raw.githubusercontent.com/biviosoftware/home-env/master/bin/install.sh | bash
biviosoftware=~/src/biviosoftware

[[ ! $(uname) =~ CYGWIN ]]
is_cygwin=$?

if [[ ! -d $biviosoftware ]]; then
    mkdir -p $biviosoftware
fi
cd $biviosoftware

for repo in home-env perl-Bivio javascript-Bivio perl-ProjEx; do
    if [[ -d $repo ]]; then
	echo "Using existing $repo"
    else
        git clone -q "https://github.com/biviosoftware/$repo"
    fi
done

if [[ ! -d ../perl ]]; then
    mkdir ../perl
fi

for perl in Bivio ProjEx; do
    if [[ -L ../perl/$perl ]]; then
        rm "../perl/$perl"
    fi
    ln -s "../biviosoftware/perl-$perl" "../perl/$perl"
done

if [[ ! -d ~/bin ]]; then
    mkdir ~/bin
fi

if [[ ! -x ~/bin/bivio ]]; then
    ln -s ~/src/biviosoftware/perl-Bivio/Util/bivio ~/bin/bivio
    chmod +x ~/bin/bivio
fi

cd home-env
# Our file names don't have spaces so ok
for f in $(perl -e 'print(map(m{^[-\w/]+\w$} ? "$_\n" : (), glob("{dot-,bin/}*")))'); do
    if [[ $f =~ ^bin/ ]]; then
        chmod +x "$f"
    fi
    src=$PWD/$f
    cmd='ln -s'
    if [[ $is_cygwin != 0 && -r cygwin/$f ]]; then
        src=$PWD/cygwin/$f
        cmd=cp
    fi
    dst=~/${f/#dot-/.}
    if [ -e "$dst" ]; then
        if cmp --silent "$dst" "$src"; then
            continue
        fi
        rm -f "$dst.old"
	mv "$dst" "$dst.old"
    fi
    $cmd "$src" "$dst"
done

# Check for dead links from previous install
for f in ~/.??* ~/bin/*; do
    if [[ -L $f && ! -e $f ]]; then
        rm "$f"
    fi
done

cd
# Final dot-file fixups/tests

# npmrc may contain credentials so need to append
if grep -q -s '^color' .npmrc; then
    : ok
else
    echo 'color = false' >> .npmrc
    chmod 600 .npmrc
fi

for dotfile in .gitconfig .netrc; do
    if [[ ! -r $dotfile ]]; then
	src=/my/$dotfile
	if [[ -r $src ]]; then
	    install --mode=0600 "$src" "$dotfile"
	else
	    echo "You need to install $dotfile"
	fi
    fi
done
