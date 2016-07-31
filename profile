#!/bin/sh

# 言語環境
export LANG=ja_JP.UTF-8
export LANGUAGE=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8

# ディレクトリ環境
export MY_LOCAL=$HOME/local
export MY_VAR=$HOME/var
export MY_SHARE=$HOME/share

# DOT環境
if [ -d "$DOTDIR" ]; then
    export PATH=$DOTDIR/bin:$PATH
fi

# goの設定
export GOPATH=$HOME/go
export GOROOT=$MY_LOCAL/go
export PATH="$GOROOT/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"

# プラットフォームごとの設定
case $(uname) in
	Darwin)
		export HOMEBREW_DIR=$MY_LOCAL/homebrew
		if [ -d "$HOMEBREW_DIR" ]; then
			export PATH="$HOMEBREW_DIR/bin:$PATH"
		fi
		;;
esac
