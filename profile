#!/bin/sh
# 環境変数を定義する

. "$DOTDIR/lib/util.sh"

# 言語環境
export LANG=ja_JP.UTF-8
export LANGUAGE=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8

# ディレクトリ環境
export MY_LOCAL="$HOME/local"
export MY_VAR="$HOME/var"
export MY_SHARE="$HOME/share"

# パス設定
_add_to_path "$DOTDIR/bin"
_add_to_path "$HOME/bin"

# プロファイルの読み込み
. "$(dotconfig profile main)"