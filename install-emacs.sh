#!/bin/sh
version=24.4
installed_version=$(emacs --version 2>&1 | perl -ne 'print(/Emacs (\d+\.\d+)/)')
if [ "x$installed_version" = "x$version" ]; then
    exit 1
fi
if rpm -q --quiet emacs-git; then
    yum remove emacs-nox emacs-common
fi
if rpm -q --quiet emacs-nox; then
    yum remove emacs-nox emacs-common
fi
if [ -d /usr/local/src/emacs-$installed_version ]; then
    ( 
        cd /usr/local/src/emacs-$installed_version
        make uninstall
    )
fi
cd /usr/local/src
wget ftp://ftp.gnu.org/pub/gnu/emacs/emacs-$version.tar.xz
tar xJf emacs-$version.tar.xz
cd emacs-$version
./configure --without-x
make install
