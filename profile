#!/bin/sh

export LANG=ja_JP.UTF-8
export LANGUAGE=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8

function _has(){
    which "$1" > /dev/null
}

if [ -d "$DOTDIR" ]; then
    export PATH=$DOTDIR/bin:$PATH
    export PAGER=less
    export EDITOR="vim"
    export FTP=ncftp
    export LESS='-X -i -R'
fi

export GOPATH=$HOME/go
export PATH="$HOME/local/go/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"

case $(uname) in
	Darwin)
		export PATH="$HOME/local/homebrew/bin:$PATH"
		;;
esac
