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
	# MacOS固有の設定
	Darwin)
		# homebrewの設定
		HOMEBREW_DIR=$MY_LOCAL/homebrew
		if [ -d "$HOMEBREW_DIR" ]; then
			export HOMEBREW_DIR
			export PATH="$HOMEBREW_DIR/bin:$PATH"
			export LD_LIBRARY_PATH="$HOMEBREW_DIR/lib:$LD_LIBRARY_PATH"
			export C_INCLUDE_PATH="$HOMEBREW_DIR/include:$C_INCLUDE_PATH"

			# LLVM関連のパスを通す
			export PATH="$HOMEBREW_DIR/opt/llvm/bin:$PATH"
			export LD_LIBRARY_PATH="$HOMEBREW_DIR/opt/llvm/lib:$LD_LIBRARY_PATH"
			export CPLUS_INCLUDE_PATH="$HOMEBREW_DIR/opt/llvm/include:$C_INCLUDE_PATH"
		fi
		# vmwareの設定
		VMWARE_APP_DIR=/Applications/VMware\ Fusion.app
		if [ -d "$VMWARE_APP_DIR" ]; then
			export VMWARE_APP_DIR
			export PATH="$VMWARE_APP_DIR/Contents/Library:$PATH"
		fi
		;;
esac
