#!/bin/sh
pwd="$(pwd)"
for b in $(perl -e 'print(map(/^dot-(\w+)$/ ? "$1\n" : (), glob("dot-*")))'); do
    src="$pwd/dot-$b"
    dst=~/.$b
    rm -f "$dst.old"
    test -e "$dst" && mv "$dst" "$dst.old"
    rm -f "$dst"
    ln -s "$src" "$dst"
done
