#!/bin/bash
set -x
. "$HOME"/.bashrc

shopt -s nullglob
for f in test/*.sh; do
    . $f
done
