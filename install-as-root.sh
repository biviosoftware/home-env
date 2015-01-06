#!/bin/sh
umask 022
cd $(dirname $0)
bash install-git.sh
bash install-emacs.sh
