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
        git clone https://github.com/biviosoftware/$repo
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
cd home-env
for dotfile in $(perl -e 'print(map(/^dot-(\w+)$/ ? "$1\n" : (), glob("dot-*")))'); do
    src="$PWD/dot-$dotfile"
    dst=~/.$dotfile
    rm -f "$dst.old"
    if [ -e "$dst" -o -L "$dst" ]; then
	mv "$dst" "$dst.old"
    fi
    ln -s "$src" "$dst"
done
cd
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
