#!/bin/bash
set -x
. ~/.bashrc

shopt -s nullglob
for f in test/*.sh; do
    . $f
done
