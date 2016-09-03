#!/bin/sh
# 環境変数を定義する

# リスト構造をもった文字列に対して指定の文字を先頭に追加する
# すでに追加されていた場合は追加しない
# $1 = 追加する文字列
# $2 = 追加対象の文字列リスト
# $3 = リストの区切り文字(デフォルトは:)
_add_to_list()
{
    case "${3:-:}$2${3:-:}" in
        *"${3:-:}$1${3:-:}"*)
            printf '%s' "$2"
            ;;
        *)
            printf '%s%s%s' "$1" "${3:-:}" "$2"
            ;;
    esac
}

# $PATH変数に対して$1のパスを追加する
# すでに追加されていた場合は追加しない
_add_to_path()
{
    PATH=$(_add_to_list "$1" "$PATH" :)
}

# profileを読みなおす
_reload_profile()
{
    . "$DOTDIR/profile"
}

# コマンドが存在するか判定する
_exits_command()
{
    which "$1" > /dev/null
}

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
    _add_to_path "$DOTDIR/bin"
fi

# プラットフォームごとの設定
case $(uname) in
	# MacOS固有の設定
	Darwin)
        # local/binにパスを通す
        _add_to_path "/usr/local/bin"
        # homebrewの設定
        if _exits_command brew; then
            export HOMEBREW_ROOT="$(brew --repo)"
            # LLVM関連のパスを通す
            _add_to_path $HOMEBREW_ROOT/opt/llvm/bin
        fi
        # vmwareの設定
        export VMWARE_APP_DIR=/Applications/VMware\ Fusion.app
        if [ -d "$VMWARE_APP_DIR" ]; then
            _add_to_path "$VMWARE_APP_DIR/Contents/Library"
        fi
        ;;
esac

# goの設定
export GOPATH="$HOME/go"
export GOROOT="$MY_LOCAL/go"
if [ -d "$GOPATH" ]; then
    _add_to_path "$GOROOT/bin"
    _add_to_path "$GOPATH/bin"
fi

# rubyの設定
export RBENV_ROOT=$HOME/local/rbenv
if [ -d "$RBENV_ROOT" ]; then
    _add_to_path "$RBENV_ROOT/bin"
    if _exits_command rbenv; then
        eval "$(rbenv init -)"
    fi
fi
