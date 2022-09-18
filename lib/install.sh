#!/bin/sh

lnk() {
    if [ -e "$2" ]; then
        echo "$1 は存在するためインストールしませんでした"
    else
        lnk_f "$@"
    fi
}

lnk_f() {
	mkdir -p "$(dirname "$1")"
    ln -fnsv "$PWD/$1" "$2"
}