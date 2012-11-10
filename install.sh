#!/bin/sh

cd $(dirname $0)
ins(){ ln -Fs "$PWD/$1" "$2" && echo install $1; }

ins .profile ~/
ins .gitconfig ~/
ins .Xdefaults ~/
