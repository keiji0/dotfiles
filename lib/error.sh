#!/bin/sh

# エラー処理を扱う関数群

fatal() {
    echo "$(basename "$0")": "$@" >&2
    exit 1
}