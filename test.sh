#!/bin/sh
. ~/.bashrc
for f in test/*.sh; do
    . $f
done
