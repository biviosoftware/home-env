#!/bin/bash
# curl -L https://raw.githubusercontent.com/biviosoftware/home-env/master/bin/install.sh | bash
h=~/src/biviosoftware/home-env 
if [ -d "$h" ]; then
    echo "$h exists; remove first:" 1>&2
    echo "rm -rf $h" 1>&2
    exit 1
fi
mkdir -p "$(dirname $h)" 2>/dev/null
cd $(dirname $h)
git clone https://github.com/biviosoftware/home-env
git clone https://github.com/biviosoftware/perl-Bivio
mkdir ../perl
ln -s ../biviosoftware/perl-Bivio ../perl/Bivio
cd $h
for b in $(perl -e 'print(map(/^dot-(\w+)$/ ? "$1\n" : (), glob("dot-*")))'); do
    src="$h/dot-$b"
    dst=~/.$b
    rm -f "$dst.old"
    test -e "$dst" && mv "$dst" "$dst.old"
    rm -f "$dst"
    ln -s "$src" "$dst"
done
