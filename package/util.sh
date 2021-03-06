#!/bin/sh
# 共通で使う関数一覧

# エラーメッセージを標準エラー出力へ出力する
error()
{
    printf 'error: %s\n' "$*" 1>&2
}

# エラーメッセージを出力してプロセスを異常終了させる
abort()
{
    error "$@"
    exit 1
}
