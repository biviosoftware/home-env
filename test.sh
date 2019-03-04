#!/bin/bash
set -x
source "$HOME"/.bashrc

shopt -s nullglob
for f in test/*.sh; do
    source $f
done
