#!/bin/bash
set -x
. ~/.bashrc
for f in test/*.sh; do
    . $f
done
