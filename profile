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
        # macportsの設定
        if [ -d "/opt/local/bin" ]; then
            export MACPORTS_ROOT="/opt/local"
            _add_to_path "$MACPORTS_ROOT/bin"
            export MANPATH=$(_add_to_list "$MACPORTS_ROOT/share/man" "$MANPATH" :)
        fi
        # local/binにパスを通す
        _add_to_path "/usr/local/bin"
        # vmwareの設定
        export VMWARE_ROOT=/Applications/VMware\ Fusion.app
        if [ -d "$VMWARE_ROOT" ]; then
            _add_to_path "$VMWARE_ROOT/Contents/Library"
        fi
        ;;
esac

# golangの設定
export GOPATH="$HOME/go"
export GOROOT="$HOME/local/go"
if [ -d "$GOPATH" ]; then
    _add_to_path "$GOPATH/bin"
    _add_to_path "$GOROOT/bin"
fi

# Android SDKの設定
export ANDROID_HOME=$HOME/Library/Android
if [ -d "$ANDROID_HOME" ]; then
    _add_to_path "$ANDROID_HOME/sdk/tools"
    _add_to_path "$ANDROID_HOME/sdk/platform-tools"
fi

# Android NDKの設定
# https://developer.android.com/ndk/downloads/index.html
if [ -d "$ANDROID_HOME/android-ndk" ]; then
    _add_to_path "$ANDROID_HOME/android-ndk"
fi

# rubyの設定
export RBENV_ROOT=$HOME/local/rbenv
if [ -d "$RBENV_ROOT" ]; then
    _add_to_path "$RBENV_ROOT/bin"
    if _exits_command rbenv; then
        eval "$(rbenv init -)"
    fi
fi

# bitcoin
export BITCOIN_ROOT=$HOME/git-extern/bitcoin
if [ -d "$BITCOIN_ROOT" ]; then
    _add_to_path "$BITCOIN_ROOT/src"
fi
