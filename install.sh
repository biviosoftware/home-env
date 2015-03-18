#!/bin/bash
# curl -L https://raw.githubusercontent.com/biviosoftware/home-env/master/bin/install.sh | bash
biviosoftware=~/src/biviosoftware
if [ ! -d $biviosoftware ]; then
    mkdir -p $biviosoftware
fi
cd $biviosoftware
for repo in home-env perl-Bivio javascript-Bivio perl-ProjEx; do
    if [ -d $repo ]; then
	echo "Using existing $repo"
    else
        git clone -q https://github.com/biviosoftware/$repo
    fi
done
if [ ! -d ../perl ]; then
    mkdir ../perl
fi
for perl in Bivio ProjEx; do
    if [ -L ../perl/$perl ]; then
        rm ../perl/$perl
    fi
    ln -s ../biviosoftware/perl-$perl ../perl/$perl
done
if [ ! -d ~/bin ]; then
    mkdir ~/bin
fi
if [ ! -x ~/bin/bivio ]; then
    ln -s ~/src/biviosoftware/perl-Bivio/Util/bivio ~/bin/bivio
    chmod +x ~/bin/bivio
fi
cd home-env
is_cygwin="$(expr "$(uname 2>/dev/null)" : '.*CYGWIN')"
for dotfile in $(perl -e 'print(map(/^dot-(\w+)$/ ? "$1\n" : (), glob("dot-*")))'); do
    src="$PWD/dot-$dotfile"
    cmd='ln -s'
    if [ "$is_cygwin" != 0 -a -r "cygwin/dot-$dotfile" ]; then
        src="$PWD/cygwin/dot-$dotfile"
        cmd='cp'
    fi
    dst=~/.$dotfile
    if [ -e "$dst" ]; then
        if cmp --silent "$dst" "$src"; then
            continue
        fi
        rm -f "$dst.old"
	mv "$dst" "$dst.old"
    fi
    $cmd "$src" "$dst"
done
cd
# npmrc may contain credentials so need to append
if grep -q -s '^color' .npmrc; then
    : ok
else
    echo 'color = false' >> .npmrc
    chmod 600 .npmrc
fi
for dotfile in .gitconfig .netrc; do
    if [ ! -r $dotfile ]; then
	src=/my/$dotfile
	if [ -r $src ]; then
	    install --mode=0600 $src $dotfile
	else
	    echo "You need to install $dotfile"
	fi
    fi
done
