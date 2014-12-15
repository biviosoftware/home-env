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
    if [ -L "$dst" ]; then
	# Don't backup symlinks, no point
	rm -f "$dst"
    elif [ -e "$dst" ]; then
        rm -f "$dst.old"
	mv "$dst" "$dst.old"
    fi
    ln -s "$src" "$dst"
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
